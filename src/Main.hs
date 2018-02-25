{-# LANGUAGE RecordWildCards, NamedFieldPuns, DeriveGeneric,
  TypeApplications #-}

import Control.Concurrent.Async (race)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ask)
import Data.Aeson hiding (json)
import Data.Either (lefts, rights)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Internal.Lazy as LText
import Data.Time.Clock (getCurrentTime)
import Database.PostgreSQL.Simple.URL (parseDatabaseUrl)
import Network.HTTP.Types
import Network.HostAndPort (hostAndPort)
import Network.Wai.Middleware.Cors (simpleCors)
import qualified System.Directory as Dir
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty

import qualified Database.Persist as P
import qualified Database.Persist.Sql as Sql
import Model
       (Database, DbBackend(..), GroupSet(..), Key, LogLevel(..),
        Page(..), Rollup(..), Run(..), RunGroup, TransactionMonad,
        WreckerRun(..), findGroupSetsByName, findGroupSetsByRunMatch,
        findPageStats, findPagesList, findRunGroups, findRunStats,
        findRuns, findRunsByMatchAndSet, runDbAction, runMigrations,
        storeRunResults, withDb)

import Control.Distributed.Process (NodeId(..), Process)
import qualified Control.Distributed.Process.Backend.SimpleLocalnet
       as LocalNet
import qualified Control.Distributed.Process.Node as Node
import Control.Distributed.Static (RemoteTable)
import Network.Transport (EndPointAddress(..))

import qualified Scheduler

-- | CLI options
data Config = Config
    { uiPort :: Int -- ^ The port for the HTTP interface
    , dbType :: DbBackend -- ^ Either Sqlite or Postgres
    , folder :: String -- ^ The path to the assets folder
    , logLevel :: LogLevel -- ^ Either Debug or Silent. Currently ony used for db queries
    , hostName :: String -- ^ The host this node is running on. Defaults to 127.0.0.1.
    , staticSlaves :: [NodeId]
    , testsList :: Scheduler.RunSchedule -- ^ Private config holding the run scheduler
    }

-- | The list of functions that are allowed to be called on remote nodes.
networkFunctions :: RemoteTable
networkFunctions = Scheduler.__remoteTable Node.initRemoteTable

main :: IO ()
main = do
    role <- lookupEnv "WRECKER_ROLE"
    case role of
        Just "slave" -> createSlave
        Just "master" -> mainProcess =<< getConfig
        Nothing -> mainProcess =<< getConfig
        _ -> error "Only master and slave roles are valid"

-- | Initializes the CLI config
getConfig :: IO Config
getConfig = do
    uiPort <- readPort "WRECKER_UI_PORT" 3000
    maybeLevel <- lookupEnv "WRECKER_LOG_LEVEL"
    let parsedLevel = maybe Nothing readMaybe maybeLevel
        logLevel = fromMaybe Silent parsedLevel
    dbType <- selectDatabaseType
    folder <- findAssetsFolder
    staticSlaves <- getStaticSlaves
    testsList <- Scheduler.emptyRunSchedule
    hostName <- fromMaybe "127.0.0.1" <$> lookupEnv "WRECKER_HOST"
    return Config {..}

-- | Starts a slave worker node. Slaves are used to execute the wrecker cli and transmit back the results
createSlave :: IO ()
createSlave = do
    port <- readPort "WRECKER_PORT" 10501
    hostName <- fromMaybe "127.0.0.1" <$> lookupEnv "WRECKER_HOST"
    backend <- LocalNet.initializeBackend hostName (show @Int port) networkFunctions
    putStrLn "Started slave process"
    LocalNet.startSlave backend

-- | Starts the local worker node. This node will always receive part of the load, specially when the
--   cocurrency level for a run is low.
mainProcess :: Config -> IO ()
mainProcess config = do
    port <- readPort "WRECKER_PORT" 10500
    hasStaticSlaves <- lookupEnv "WRECKER_SLAVES"
    putStrLn "Starting local node"
    backend <- LocalNet.initializeBackend (hostName config) (show @Int port) networkFunctions
    case hasStaticSlaves of
        Nothing -> LocalNet.startMaster backend (startUI config)
        _ -> do
            putStrLn "Found a static list of slaves"
            node <- LocalNet.newLocalNode backend
            Node.runProcess node (startUI config [])

-- | Starts the program itself. That is, the http interface and the run scheduler
startUI :: Config -> [NodeId] -> Process ()
startUI Config {..} discoveredSlaves = do
    liftIO $ putStrLn "Starting Web UI"
    let slaves = staticSlaves ++ discoveredSlaves
    localProcess <- ask -- Get the context that the Process monad is enclosing
    liftIO $ do
        putStrLn ("Found the following slave servers: " ++ show slaves)
        withDb logLevel dbType $ \db ->
            liftIO $ -- We're inside the DbMonad, so the results needs to be converted back to IO
             do
                runMigrations db -- Run the database migrations first
                void $ -- Disregard the return value of `race`
                    race -- start both function in different threads and stop the other if one dies
                        (Scheduler.runScheduler Scheduler.Config {..}) -- start accepting new jobs
                        (routes uiPort folder db testsList) -- start accepting http requests

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
            let protocolFixedInfo = Text.replace "postgresql://" "postgres://" (Text.pack info)
            in case parseDatabaseUrl (Text.unpack protocolFixedInfo) of
                   Nothing -> error "Invalid WRECKER_DB url"
                   Just connDetails -> do
                       putStrLn "Using PostgreSQL"
                       return (Postgresql connDetails)

getStaticSlaves :: IO [NodeId]
getStaticSlaves = do
    maybeString <- lookupEnv "WRECKER_SLAVES"
    case maybeString of
        Nothing -> return []
        Just "" -> return []
        Just str -> do
            let parsed = parseNodes (Text.pack str)
                builder s = NodeId (EndPointAddress (encodeUtf8 (s <> ":0")))
            mapM_ (putStrLn . Text.unpack) (lefts parsed) -- Let the user know there were errors in slaves
            return (fmap (builder . Text.pack) (rights parsed))
  where
    parseNodes str =
        let splitted = filter (/= Text.empty) (Text.strip <$> Text.splitOn "," str)
        in fmap (ensurePort . validate) splitted
    validate s = (s, hostAndPort (Text.unpack s))
    ensurePort (s, Left _) = Left ("Invalid slave address: " <> s)
    ensurePort (s, Right (_, Nothing)) = Left ("Invalid slave address. Missing port for: " <> s)
    ensurePort (_, Right (host, Just port)) = Right (host <> ":" <> port)

----------------------------------
-- Routes and middleware
----------------------------------
routes :: Int -> String -> Database -> Scheduler.RunSchedule -> IO ()
routes port folder db testsList =
    scotty port $ -- Let's declare the routes and handlers
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
        post "/runs/:id" (storeResults db)
        --  ^ Gets the JSON blob from a wrecker run and stores them in the DB
        get "/runs/rollup" (getManyRuns db)
        --  ^ Returns the basic info for the passed runs, the list of pages and the general stats
        get "/runs/page" (getPageStats db)
        --  ^ Returns the statistics for a single or multiple runs
        get "/runs/:id" (getRun db)
        --  ^ Returns the basic info for the run, the list of pages and the general stats
        get "/test-list" (getTestList testsList)
        --  ^ Returns the list of tests with the status of the last run for them if any
        post "/test-list" (scheduleTest testsList)
        --  ^ Returns the list of tests with the status of the last run for them if any
        get "/group-sets" (listGroupSets db)
        --  ^ Returns the list of tests group sets
        post "/group-sets" (storeGroupSet db)
        --  ^ Saves a group set in the database

----------------------------------
-- Controllers
----------------------------------
listRuns :: Database -> ActionM ()
listRuns db = do
    match <- optionalParam "match"
    groupSet <- readMaybe <$> optionalParam "groupSet"
    result <- liftAndCatchIO (fetchRuns match groupSet)
    json $ object ["runs" .= result, "success" .= True]
  where
    fetchRuns :: Text -> Maybe Int -> IO [P.Entity Run]
    fetchRuns match groupSet = runDbAction db $ findRunsByMatchAndSet match groupSet Nothing

listGroupSets :: Database -> ActionM ()
listGroupSets db = do
    name <- optionalParam "name"
    result <- liftAndCatchIO (fetchSets name)
    json $ object ["groupSets" .= result, "success" .= True]
  where
    fetchSets :: Text -> IO [P.Entity GroupSet]
    fetchSets name = runDbAction db $ findGroupSetsByName name

storeGroupSet :: Database -> ActionM ()
storeGroupSet db = do
    name <- param "name"
    description <- param "description"
    liftAndCatchIO (storeSet name description)
    json $ object ["success" .= True]
    status ok200
  where
    storeSet :: Text -> Text -> IO ()
    storeSet name desc = do
        now <- getCurrentTime
        runDbAction db $
            P.insert_
                GroupSet {groupSetName = name, groupSetDescription = desc, groupSetCreated = now}

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
            (stats, pages, runGroups) <- liftAndCatchIO (runDbAction db $ fetchRunStats [runId])
            -- if the list is empty, errorResponse, otherwise call sendResult with the first in the list
            maybe (errorResponse errorTxt) (sendResult pages runGroups) (listToMaybe stats)
        Left err -> errorResponse err
  where
    errorTxt :: Text
    errorTxt = "No query result"
    -- | Transforms the SQL result into a JSON response
    --
    sendResult list runGroups (run, stats) = do
        json $ object ["run" .= run, "stats" .= stats, "pages" .= list, "runGroups" .= runGroups]
        status ok200
    -- | Responds with a JSON error
    --
    errorResponse err = do
        json $ object ["error" .= ("Invalid run id" :: Text), "reason" .= err]
        status badRequest400

-- | Fetches the run stats in the datatabase. It retuns a pair:
--   The first positionin the pair is a list of (run, rollup)
--   The second position is a reverse index of pages participating in the run {page: [run id, ...]}
fetchRunStats ::
       MonadIO m
    => [Int]
    -> TransactionMonad m ([(P.Entity Run, Rollup)], Map.Map Text [Key Run], [P.Entity RunGroup])
fetchRunStats runId = do
    let runKeys = toSqlKey <$> runId
    stats <- findRunStats runKeys
    list <- findPagesList runKeys
    runs <- findRuns (fmap fst stats)
    runGroups <- findRunGroups runKeys
    return
        ( [ (run, rollup) -- Locally doing a join by the same run key
          | (sKey, rollup) <- stats
          , run@(P.Entity rKey _) <- runs
          , sKey == rKey
          ]
        , reverseIndex list
        , runGroups)
  where
    reverseIndex pages = Map.fromListWith (++) [(page, [key]) | (key, page) <- pages]

getManyRuns :: Database -> ActionM ()
getManyRuns db = do
    allIds <- idsOrMatch db
    match <- optionalParam "match"
    case allIds of
        [] -> errorResponse "Invalid list of ids"
        _ -> do
            (result, sets) <-
                liftAndCatchIO $
                runDbAction db $ do
                    stats <- fetchRunStats allIds
                    if match == ""
                        then return (stats, Nothing)
                        else do
                            availableSets <- findGroupSetsByRunMatch match
                            return (stats, Just availableSets)
            sendResult result sets
  where
    sendResult (results, pages, runGroups) sets = do
        let buildObject (run, stats) = object ["run" .= run, "stats" .= stats]
        json $
            object
                [ "runs" .= fmap buildObject results
                , "pages" .= pages
                , "runGroups" .= runGroups
                , "availableGroupSets" .= sets
                ]
        status ok200
    -- | Responds with a JSON error
    --
    errorResponse :: Text -> ActionM ()
    errorResponse err = do
        json $ object ["reason" .= err]
        status badRequest400

getPageStats :: Database -> ActionM ()
getPageStats db = do
    allIds <- idsOrMatch db
    pageName <- param "name"
    page <- liftAndCatchIO (fetchPageStats allIds pageName)
    json page
  where
    fetchPageStats :: [Int] -> Text -> IO [Page]
    fetchPageStats runIds pageName =
        runDbAction db $ do
            let runKeys = toSqlKey <$> runIds
            findPageStats runKeys pageName

getTestList :: Scheduler.RunSchedule -> ActionM ()
getTestList testsList = do
    tests <- liftAndCatchIO $ Scheduler.getRunSchedule testsList
    json $ object ["status" .= True, "tests" .= tests]

scheduleTest :: Scheduler.RunSchedule -> ActionM ()
scheduleTest testsList = do
    testTitle <- param "testTitle"
    groupName <- param "groupName"
    cStart <- readEither <$> param "concurrencyStart"
    cEnd <- readEither <$> param "concurrencyEnd"
    sSize <- readEither <$> param "stepSize"
    time <- Right . readMaybe <$> param "runTime" -- Read into a Maybe, then wrap with Right
    groupSetId <- Right . readMaybe <$> optionalParam "groupSetId" -- Read into a Maybe, then wrap with Right
    notes <- Right <$> param "notes"
    --
    -- The lazy way of checking for conversion errors
    -- We "unpack" the "Right" value from each var. If any "Left" is found
    -- The operation is aborted and the whole subroutine retuns "Left"
    let allParams =
            Scheduler.ScheduleOptions groupName <$> -- Build the options as we check their validity
            cStart <*>
            cEnd <*>
            sSize <*>
            time <*>
            notes <*>
            groupSetId
    case allParams of
        Left err -> handleError err
        Right schedule -> do
            result <- liftAndCatchIO (Scheduler.addToQueue testTitle schedule testsList)
            either handleError handleSucccess result
  where
    handleError err = do
        json $ object ["error" .= ("Invalid argument" :: Text), "reason" .= err]
        status badRequest400
    handleSucccess _ = do
        json $ object ["success" .= True]
        status created201

----------------------------------
-- Utilities
----------------------------------
readPort :: Read port => String -> port -> IO port
readPort name defaultPort = do
    taintedPort <- lookupEnv name
    -- Reading the port from the ENV. Abort with an error on bad port
    let varReader = fromMaybe (error $ "Bad " <> name)
    return (maybe defaultPort (varReader . readMaybe) taintedPort)

optionalParam :: (Monoid a, Parsable a) => LText.Text -> ActionM a
optionalParam name = param name `rescue` (\_ -> return mempty)

parseIdList :: ActionM [Int]
parseIdList = do
    longString <- optionalParam "ids"
    let idsList = fmap Text.unpack (Text.splitOn "," longString)
        parsedList = fmap readMaybe idsList
    return (catMaybes parsedList)

idsOrMatch :: Database -> ActionM [Int]
idsOrMatch db = do
    ids <- parseIdList
    case ids of
        [] -> idsFromMatch
        _ -> return ids
  where
    idsFromMatch = do
        match <- param "match"
        runGroupLimit <- readMaybe <$> optionalParam "groupLimit"
        groupSet <- readMaybe <$> optionalParam "groupSet"
        runs <- liftAndCatchIO (runDbAction db $ findRunsByMatchAndSet match groupSet runGroupLimit)
        return (fmap (fromIntegral . Sql.fromSqlKey . P.entityKey) runs)

toSqlKey :: Int -> Key Run
toSqlKey = Sql.toSqlKey . fromIntegral
