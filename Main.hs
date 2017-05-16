{-# LANGUAGE RecordWildCards, NamedFieldPuns, DeriveGeneric #-}

import Control.Concurrent.Async (async, poll, Async)
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (json)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import qualified Data.Text.Internal.Lazy as LText
import Data.Time.Clock (getCurrentTime)
import Database.PostgreSQL.Simple.URL (parseDatabaseUrl)
import qualified Invoker as Wrecker
import Network.HTTP.Types
import Network.Wai.Middleware.Cors (simpleCors)
import Prelude hiding (id)
import qualified System.Directory as Dir
import System.Environment (lookupEnv)
import Web.Scotty

import qualified Database.Persist as P
import qualified Database.Persist.Sql as Sql
import Model
       (Page(..), Run(..), Rollup(..), DbBackend(..), Database, Key,
        RunInfo(..), WreckerRun(..), withDb, runDbAction, runMigrations,
        findRuns, findRunStats, findPagesList, findPageStats,
        storeRunResults)

data RunStatus
  = Running (Maybe (Async ()))
  | Done
  | Scheduled ScheduleOptions
  | None

data ScheduleOptions = ScheduleOptions
  { gName :: Text
  , cStart :: Int
  , cEnd :: Int
  , sStep :: Int
  }

type RunSchedule = STM.TVar (Map Text RunStatus)

main :: IO ()
main = do
  dbType <- selectDatabaseType
  folder <- findAssetsFolder
  testsList <- emptyRunSchedule
  withDb dbType $ \db ->
    liftIO $ -- We're inside the DbMonad, so the results needs to be converted back to IO
     do
      runMigrations db
      routes folder db testsList

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

selectDatabaseType :: IO DbBackend
selectDatabaseType = do
  dbInfo <- lookupEnv "WRECKER_DB"
  case dbInfo of
    Nothing -> do
      putStrLn "Using SQLite"
      return Sqlite
    Just info ->
      case parseDatabaseUrl info of
        Nothing -> error "Invalid WRECKER_DB url"
        Just connDetails -> do
          putStrLn "Using PostgreSQL"
          return (Postgresql connDetails)

emptyRunSchedule :: IO RunSchedule
emptyRunSchedule = do
  list <- Wrecker.listGroups
  STM.newTVarIO (Map.fromList [(pack t, None) | t <- list])

----------------------------------
-- Routes and middleware
----------------------------------
routes :: String -> Database -> RunSchedule -> IO ()
routes folder db testsList = do
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
    get "/runs" (listRuns db)
    --  ^ Gets the list of runs that have been stored. Optionally filtering by "match"
    post "/runs" (createRun db)
    --  ^ Creates a new run and returns the id
    post "/runs/:id" (storeResults db)
    --  ^ Gets the JSON blob from a wrecker run and stores them in the DB
    get "/runs/:id" (getRun db)
    --  ^ Returns the basic info for the run, the list of pages and the general stats
    get "/runs/:id/page" (getPageStats db)
    --  ^ Returns the statistics for a specific page in a run
    get "/test-list" (getTestList testsList)
    --  ^ Returns the list of tests with the status of the last run for them if any
    post "/test-list" (scheduleTest db testsList)

----------------------------------
-- Controllers
----------------------------------
listRuns :: Database -> ActionM ()
listRuns db = do
  match <- optionalParam "match"
  result <- liftAndCatchIO (fetchRuns match)
  json $ object ["runs" .= result, "success" .= True]
  where
    fetchRuns :: Text -> IO [RunInfo]
    fetchRuns match =
      runDbAction db $ do
        entities <- findRuns match
        return $ fmap extractAndConvert entities
    extractAndConvert e =
      let key = fromIntegral . Sql.fromSqlKey . Sql.entityKey $ e
      in RunInfo key (Sql.entityVal e)

createRun :: Database -> ActionM ()
createRun db = do
  conc <- readEither <$> param "concurrency"
  title <- param "title"
  group <- optionalParam "groupName"
  case conc of
    Left err -> do
      json $ object ["error" .= ("Invalid concurrency number" :: Text), "reason" .= err]
      status badRequest400
    Right concurrency -> do
      now <- liftAndCatchIO getCurrentTime
      let run = Run title group concurrency now
      newId <- liftAndCatchIO (doStoreRun run)
      json $ object ["success" .= True, "id" .= newId]
      status created201
  where
    doStoreRun :: Run -> IO (Key Run)
    doStoreRun run = runDbAction db (P.insert run)

storeResults :: Database -> ActionM ()
storeResults db = do
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
    storeStats runId run = runDbAction db $ storeRunResults (toSqlKey runId) run

getRun :: Database -> ActionM ()
getRun db = do
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
    fetchRunStats :: Int -> IO (Maybe (RunInfo, Rollup, [Text]))
    fetchRunStats runId =
      runDbAction db $ do
        let runKey = toSqlKey runId
        stats <- findRunStats runKey
        list <- findPagesList runKey
        run <- P.get runKey
        return $
          case (run, stats) of
            (Nothing, _) -> Nothing
            (_, []) -> Nothing
            (Just r, statistics:_) -> Just (RunInfo runId r, statistics, list)

getPageStats :: Database -> ActionM ()
getPageStats db = do
  runId <- param "id"
  pageName <- param "name"
  page <- liftAndCatchIO (fetchPageStats runId pageName)
  json page
  where
    fetchPageStats :: Int -> Text -> IO (Maybe Page)
    fetchPageStats runId pageName =
      runDbAction db $ do
        let runKey = toSqlKey runId
        result <- findPageStats runKey pageName
        return $
          case result of
            [] -> Nothing
            pageStats:_ -> Just pageStats

getTestList :: RunSchedule -> ActionM ()
getTestList testsList = do
  tests <- liftAndCatchIO $ STM.atomically (STM.readTVar testsList)
  json $ object ["status" .= True, "tests" .= tests]

scheduleTest :: Database -> RunSchedule -> ActionM ()
scheduleTest db testsList = do
  testTitle <- param "testTitle"
  groupName <- param "groupName"
  cStart <- readEither <$> param "concurrencyStart"
  cEnd <- readEither <$> param "concurrencyEnd"
  sSize <- readEither <$> param "stepSize"
  --
  -- The lazy way of checking for conversion errors
  -- We "unpack" the "Right" value from each var. If any "Left" is found
  -- The operation is aborted and the whole subroutine retuns "Left"
  let allParams = (ScheduleOptions groupName) <$> cStart <*> cEnd <*> sSize
  case allParams of
    Left err -> handleError err
    Right schedule -> do
      result <- liftAndCatchIO (doScheduling db testTitle schedule testsList)
      either handleError handleSucccess result
  where
    handleError err = do
      json $ object ["error" .= ("Invalid argument" :: Text), "reason" .= err]
      status badRequest400
    handleSucccess _ = do
      json $ object ["success" .= True]
      status created201

doScheduling :: Database
             -> Text
             -> ScheduleOptions
             -> STM.TVar (Map Text RunStatus)
             -> IO (Either Text Bool)
doScheduling db name schedule@ScheduleOptions {..} testsList = do
  result <- STM.atomically changeList
  case result of
    Right True -> do
      job <- async (Wrecker.escalate db gName name [cStart,sStep .. cEnd])
      STM.atomically (updateStatus (Running $ Just job))
      return result
    _ -> return result
  where
    updateStatus :: RunStatus -> STM.STM ()
    updateStatus s = STM.modifyTVar testsList $ Map.insert name s
    -- | Modifies the test list if it is valid to insert the new schedule
    --
    changeList :: STM.STM (Either Text Bool)
    changeList = do
      tests <- STM.readTVar testsList
      case canExecute tests of
        Left e -> return (Left e)
        Right immediate -> do
          let val =
                if immediate
                  then (Running Nothing)
                  else (Scheduled schedule)
          updateStatus val
          return $ Right immediate
    -- | Returns whehter it is ok to schedule or run the fiven test
    --   Left is returned when it is not possible to run the test.
    --   "Right True" is returned when the test can be executed immediately
    --   Otherwise the test is ok to be set for scheduling
    --
    canExecute list =
      case Map.lookup name list of
        Nothing -> Left "Test name does not exist"
        Just (Running _) -> Left "This test is still running"
        Just (Scheduled _) -> Left "This test is already scheduled"
        Just _ -> Right (nothingIsRuning list)
    -- | Checks that none of the tests in the list have a Running status
    --
    nothingIsRuning =
      all
        (\tStatus ->
           case tStatus of
             Running _ -> False
             _ -> True)

----------------------------------
-- Utilities
----------------------------------
optionalParam
  :: (Monoid a, Parsable a)
  => LText.Text -> ActionM a
optionalParam name = param name `rescue` (\_ -> return mempty)

toSqlKey :: Int -> Key Run
toSqlKey = Sql.toSqlKey . fromIntegral

----------------------------------
-- JSON Conversions
----------------------------------
instance ToJSON RunStatus where
  toJSON (Running _) = String "running"
  toJSON Done = String "done"
  toJSON (Scheduled _) = String "scheduled"
  toJSON None = String "none"
