{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns,
  DeriveGeneric #-}

import Data.Aeson hiding (json)
import Data.Aeson.Types (Parser)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.IO as T (readFile)
import qualified Data.Text.Internal.Lazy as LText
import Data.Time.Clock (UTCTime)
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple (ToRow, FromRow)
import Database.SQLite.Simple.ToField (ToField(..))
import GHC.Generics
import Network.HTTP.Types
import Network.Wai.Middleware.Cors (simpleCors)
import Prelude hiding (id)
import qualified System.Directory as Dir
import System.Environment (lookupEnv)
import Web.Scotty

data WreckerRun = WreckerRun
  { rollup :: Statistics
  , pages :: [Page]
  } deriving (Eq, Show)

data Statistics = Statistics
  { hits :: Int
  , maxTime :: Double
  , meanTime :: Double
  , minTime :: Double
  , totalTime :: Double
  , variance :: Double
  , successHits :: Int
  , userErrorHits :: Int
  , serverErrorHits :: Int
  , failedHits :: Int
  , quantile95 :: Double
  } deriving (Eq, Show, Generic)

newtype PageUrl =
  PageUrl Text
  deriving (Eq, Show, Generic)

data Page = Page
  { url :: PageUrl
  , stats :: Statistics
  } deriving (Eq, Show, Generic)

newtype SQLRollup =
  SQLRollup (Int, Statistics)

newtype SQLPage =
  SQLPage (Int, Page)

data RunInfo = RunInfo
  { id :: Int
  , match :: Text
  , groupName :: Text
  , concurrency :: Int
  , created :: UTCTime
  } deriving (Eq, Show, Generic)

main :: IO ()
main = do
  folder <- findAssetsFolder
  conn <- initDB folder
  scotty 3000 $ -- Let's declare the routes and handlers
   do
    middleware simpleCors
    -- ^ Useful when developing the elm app using a separate elm-live server
    get "/" $ do
      setHeader "Content-Type" "text/html; charset=utf-8"
      file (folder <> "index.html")
    get "/app.js" $ do
      setHeader "Content-Type" "application/javascript"
      file (folder <> "app.js")
    get "/main.css" $ do
      setHeader "Content-Type" "text/css"
      file (folder <> "main.css")
    -- ^ Serves the few static files we have
    get "/runs" (listRuns conn)
    --  ^ Gets the list of runs that have been stored. Optionally filtering by "match"
    post "/runs" (createRun conn)
    --  ^ Creates a new run and returns the id
    post "/runs/:id" (storeResults conn)
    --  ^ Gets the JSON blob from a wrecker run and stores them in the DB
    get "/runs/:id" (getRun conn)
    --  ^ Returns the basic info for the run, the list of pages and the general stats
    get "/runs/:id/page" (getPageStats conn)

----------------------------------
-- App initialization
----------------------------------
findAssetsFolder :: IO String
findAssetsFolder = do
  f <- lookupEnv "WRECKER_ASSETS"
  let folder =
        case f of
          Nothing -> "assets/"
          Just path -> path <> "/"
  exists <- Dir.doesFileExist (folder <> "app.js")
  if not exists
    then error
           "Could not find the assets folder. Set the WRECKER_ASSETS env variable with the full path to it"
    else return folder

initDB :: String -> IO SQL.Connection
initDB base = do
  conn <- SQL.open "wrecker.db"
  tables <- SQL.query_ conn "SELECT name FROM sqlite_master ORDER BY name" :: IO [[Text]]
  if length tables < 3
    then do
      schema <- T.readFile (base <> "schema.sql")
      let statements = filter (/= Text.empty) (Text.splitOn ";\n" $ schema)
      mapM_ (SQL.execute_ conn . fromString . Text.unpack) statements
      return conn
    else return conn

----------------------------------
-- Controllers
----------------------------------
listRuns :: SQL.Connection -> ActionM ()
listRuns conn = do
  match <- optionalParam "match"
  result <- liftAndCatchIO (fetchRuns match)
  json $ object ["runs" .= result, "success" .= True]
  where
    fetchRuns :: Text -> IO [RunInfo]
    fetchRuns match = SQL.query conn listRunsQuery ["%" <> match <> "%"]

createRun :: SQL.Connection -> ActionM ()
createRun conn = do
  conc <- readEither <$> param "concurrency"
  title <- param "title"
  group <- optionalParam "groupName"
  case conc of
    Left err -> do
      json $ object ["error" .= ("Invalid concurrency number" :: Text), "reason" .= err]
      status badRequest400
    Right concurrency -> do
      newId <- liftAndCatchIO (doStoreRun concurrency title group)
      json $ object ["success" .= True, "id" .= newId]
      status created201
  where
    doStoreRun :: Int -> Text -> Text -> IO Int
    doStoreRun concurrency title group =
      SQL.withTransaction conn $ do
        SQL.execute conn insertRunQuery (concurrency, title, group)
        newId <- fromIntegral <$> SQL.lastInsertRowId conn
        return newId

storeResults :: SQL.Connection -> ActionM ()
storeResults conn = do
  runId <- param "id"
  jsonPayload <- body
  let wreckerRun = eitherDecode jsonPayload :: Either String WreckerRun
  case wreckerRun of
    Left err -- In case we can't parse the json, respond with error
     -> do
      json $ object ["error" .= ("Invalid JSON payload" :: Text), "reason" .= err]
      status badRequest400
    Right run -- Otherwise, we have the run and we can store it in the DB
     -> do
      liftAndCatchIO (storeStats runId run)
      json $ object ["success" .= True]
      status ok200
  where
    storeStats :: Int -> WreckerRun -> IO ()
    storeStats runId WreckerRun {rollup, pages} =
      SQL.withTransaction conn $ do
        SQL.execute conn insertRollupQuery (SQLRollup (runId, rollup))
        --
        -- After Inserting the rollup, we insert all the pages at once
        -- using a single statement
        let sqlPages = fmap (\p -> SQLPage (runId, p)) pages
        SQL.executeMany conn insertPageQuery sqlPages -- Traverse all pages and insert them

getRun :: SQL.Connection -> ActionM ()
getRun conn = do
  rId <- readEither <$> param "id"
  case rId of
    Right runId -> do
      result <- liftAndCatchIO (fetchRunStats runId)
      maybe (errorResponse errorTxt) sendResult result
    Left err -> errorResponse err
  where
    errorTxt :: Text
    errorTxt = "No query result"
    -- | Transforms the SQL result into a JSON response
    --
    sendResult (run, stats, list) = do
      json $ object ["run" .= run, "stats" .= stats, "pages" .= list]
      status ok200
    -- | Responds with a JSON error
    --
    errorResponse err = do
      json $ object ["error" .= ("Invalid run id" :: Text), "reason" .= err]
      status badRequest400
    -- | Fetches the run stats in sqlite
    --
    fetchRunStats :: Int -> IO (Maybe (RunInfo, Statistics, [PageUrl]))
    fetchRunStats runId = do
      stats <- SQL.query conn runStatsQuery [runId]
      list <- SQL.query conn pagesListQuery [runId]
      run <- SQL.query conn fetchRunQuery [runId]
      return $
        case stats of
          [] -> Nothing
          statistics:_ -> Just (head run, statistics, list)

getPageStats :: SQL.Connection -> ActionM ()
getPageStats conn = do
  runId <- param "id"
  pageName <- param "name"
  page <- liftAndCatchIO (fetchPageStats runId pageName)
  json page
  where
    fetchPageStats :: Int -> Text -> IO (Maybe Page)
    fetchPageStats runId pageName = do
      result <- SQL.query conn fetchPageStatsQuery (runId, pageName)
      return $
        case result of
          [] -> Nothing
          pageStats:_ -> Just pageStats

----------------------------------
-- Utilities
----------------------------------
optionalParam
  :: (Monoid a, Parsable a)
  => LText.Text -> ActionM a
optionalParam name = param name `rescue` (\_ -> return mempty)

----------------------------------
-- SQL Queries
----------------------------------
insertRunQuery :: SQL.Query
insertRunQuery =
  "INSERT INTO runs (concurrency, test_match, group_name, created) VALUES (?, ?, ?, DATETIME('now'))"

insertRollupQuery :: SQL.Query
insertRollupQuery =
  "INSERT INTO rollups (run_id, hits, max_time, mean_time, min_time, total_time, var_time, " <>
  "hits_2xx, hits_4xx, hits_5xx, failed, quantile_95) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

insertPageQuery :: SQL.Query
insertPageQuery =
  "INSERT INTO pages (run_id, page_url, hits, max_time, mean_time, min_time, total_time, var_time, " <>
  "hits_2xx, hits_4xx, hits_5xx, failed, quantile_95) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

runStatsQuery :: SQL.Query
runStatsQuery =
  "SELECT SUM(hits), MAX(max_time), AVG(mean_time), MIN(min_time)," <>
  "       SUM(total_time), AVG(var_time), SUM(hits_2xx), SUM(hits_4xx)," <>
  "       SUM(hits_5xx), SUM(`failed`), AVG(quantile_95) " <>
  "FROM rollups " <>
  "WHERE run_id = ? " <>
  "GROUP BY run_id"

pagesListQuery :: SQL.Query
pagesListQuery = "SELECT DISTINCT page_url FROM pages where run_id = ?"

listRunsQuery :: SQL.Query
listRunsQuery =
  "SELECT id, test_match, group_name, concurrency, created FROM runs WHERE test_match LIKE ? ORDER BY created DESC"

fetchRunQuery :: SQL.Query
fetchRunQuery = "SELECT id, test_match, group_name, concurrency, created FROM runs WHERE id = ?"

fetchPageStatsQuery :: SQL.Query
fetchPageStatsQuery =
  "SELECT SUM(hits), MAX(max_time), AVG(mean_time), MIN(min_time)," <>
  "       SUM(total_time), AVG(var_time), SUM(hits_2xx), SUM(hits_4xx)," <>
  "       SUM(hits_5xx), SUM(`failed`), AVG(quantile_95)," <>
  "       page_url " <>
  "FROM pages " <>
  "WHERE run_id = ? AND page_url = ?" <>
  "GROUP BY page_url"

----------------------------------
-- SQL Conversions
----------------------------------
instance ToRow SQLRollup where
  toRow (SQLRollup (runId, Statistics {..})) =
    [ toField runId
    , toField hits
    , toField maxTime
    , toField meanTime
    , toField minTime
    , toField totalTime
    , toField variance
    , toField successHits
    , toField userErrorHits
    , toField serverErrorHits
    , toField failedHits
    , toField quantile95
    ]

instance ToRow SQLPage where
  toRow (SQLPage (runId, Page {url = PageUrl u, stats = Statistics {..}})) =
    [ toField runId
    , toField u
    , toField hits
    , toField maxTime
    , toField meanTime
    , toField minTime
    , toField totalTime
    , toField variance
    , toField successHits
    , toField userErrorHits
    , toField serverErrorHits
    , toField failedHits
    , toField quantile95
    ]

instance FromRow PageUrl where
  fromRow = PageUrl <$> SQL.field

instance FromRow Statistics where
  fromRow =
    Statistics <$> -- Behold the parser for 11 fields
    SQL.field <*>
    SQL.field <*>
    SQL.field <*>
    SQL.field <*>
    SQL.field <*>
    SQL.field <*>
    SQL.field <*>
    SQL.field <*>
    SQL.field <*>
    SQL.field <*>
    SQL.field -- Type inference will map the 'field' funciton to the right type

instance FromRow RunInfo where
  fromRow = do
    id <- SQL.field
    match <- SQL.field
    groupName <- SQL.field
    concurrency <- SQL.field
    created <- SQL.field
    return RunInfo {..}

instance FromRow Page where
  fromRow = do
    hits <- SQL.field
    maxTime <- SQL.field
    meanTime <- SQL.field
    minTime <- SQL.field
    totalTime <- SQL.field
    variance <- SQL.field
    successHits <- SQL.field
    userErrorHits <- SQL.field
    serverErrorHits <- SQL.field
    failedHits <- SQL.field
    quantile95 <- SQL.field
    url <- SQL.field
    return $ Page {url = PageUrl url, stats = Statistics {..}}

----------------------------------
-- JSON Conversions
----------------------------------
instance FromJSON Statistics where
  parseJSON =
    withObject "Statistics" $ \o -> do
      rollup <- o .: "rollup"
      maxTime <- rollup .: "max"
      meanTime <- rollup .: "mean"
      minTime <- rollup .: "min"
      hits <- rollup .: "count"
      variance <- rollup .: "variance"
      totalTime <- rollup .: "total"
      quantile95 <- rollup .: "quantile95"
      successData <- o .: "2xx"
      successHits <- successData .: "count"
      userErrorData <- o .: "4xx"
      userErrorHits <- userErrorData .: "count"
      serverErrorData <- o .: "5xx"
      serverErrorHits <- serverErrorData .: "count"
      failureData <- o .: "failed"
      failedHits <- failureData .: "count"
      return Statistics {..}

instance FromJSON WreckerRun where
  parseJSON =
    withObject "WreckerRun" $ \o -> do
      rollup <- o .: "runs"
      allPages <- o .: "per-request" :: Parser (Map Text Statistics)
      -- Now we convert the pages dictionary into a list of 'Page'
      -- by traversing all the structure with a accumulator function
      let pages =
            Map.foldlWithKey'
              (\list u stats ->
                 let url = PageUrl u
                 in Page {..} : list)
              [] -- The initial accumulator value
              allPages
      return WreckerRun {..}

instance ToJSON PageUrl

instance ToJSON Statistics

instance ToJSON RunInfo

instance ToJSON Page
