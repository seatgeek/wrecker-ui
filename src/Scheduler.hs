{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Scheduler
    ( RunSchedule
    , Config(..)
    , ScheduleOptions(..)
    , emptyRunSchedule
    , runScheduler
    , getRunSchedule
    , addToQueue
    , __remoteTable -- This is created by the call to remotable using TemplateHaskell
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, pollSTM)
import qualified Control.Concurrent.STM as STM
import Control.Monad (guard)
import Data.Aeson
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Maybe as Maybe
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Time.Clock (getCurrentTime)
import Data.Time.ISO8601 (formatISO8601)
import qualified Invoker as Wrecker
import Model (Database, WreckerRun)
import qualified Recorder

import Control.Distributed.Process (NodeId, Process, call, liftIO)
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Internal.Types
       (LocalProcess, runLocalProcess)

data RunStatus
    = Running (Maybe (Async Wrecker.Result))
    | Done
    | Scheduled ScheduleOptions
    | None

data ScheduleOptions = ScheduleOptions
    { gName :: Text
    , cStart :: Int
    , cEnd :: Int
    , sStep :: Int
    , time :: Maybe Int
    } deriving (Show)

type RunSchedule = STM.TVar (Map Text RunStatus)

data Config = Config
    { db :: Database
    , testsList :: RunSchedule
    , localProcess :: LocalProcess
    , slaves :: [NodeId]
    }

wrecker :: Wrecker.Command -> Process (Either String WreckerRun)
wrecker = liftIO . Wrecker.wrecker

remotable ['wrecker]

emptyRunSchedule :: IO RunSchedule
emptyRunSchedule = do
    list <- Wrecker.listGroups
    STM.newTVarIO (Map.fromList [(pack t, None) | t <- list])

getRunSchedule :: STM.TVar a -> IO a
getRunSchedule testsList = STM.atomically (STM.readTVar testsList)

runScheduler :: Config -> IO ()
runScheduler config@(Config {..}) = do
    threadDelay $ 5 * 1000 * 1000 -- wait 5 seconds
    maybeSchedule <-
        STM.atomically $ do
            tests <- STM.readTVar testsList
            cleaned <- cleanupFinished tests
            STM.writeTVar testsList cleaned
            pickOneToRun
    process maybeSchedule
    runScheduler (Config {..})
  where
    cleanupFinished tests = do
        let running = filter isRunning (Map.toList tests)
        polled <- mapM pollTest running
        let updated = Map.fromList (Maybe.mapMaybe markAsDone polled)
        return (Map.union updated tests)
    --
    -- | In the pair (title, job) poll the job for a status and return it
    pollTest (title, Running (Just stat)) = do
        res <- pollSTM stat
        return (title, res)
    pollTest (title, _) = return (title, Nothing)
    --
    -- | In the pair (title, status) if status is not Nothing, replace
    --   the status with "Done", otherwise returns Nothing
    markAsDone (_, Nothing) = Nothing
    markAsDone (title, _) = Just (title, Done)
    pickOneToRun = do
        tests <- STM.readTVar testsList
        return (take 1 $ filter isScheduled (Map.toList tests))
    --
    -- | Selects elements having Scheduled as status
    isScheduled element =
        case element of
            (_, Scheduled _) -> True
            _ -> False
    --
    -- | Selects elements having Running as status
    isRunning element =
        case element of
            (_, Running _) -> True
            _ -> False
    --
    -- | If any test was selected for running, then run it
    process maybeSchedule =
        case maybeSchedule of
            (name, Scheduled schedule):_ -> tryRunningNow config name schedule
            _ -> return ()

addToQueue :: Text -> ScheduleOptions -> RunSchedule -> IO (Either Text Bool)
addToQueue name schedule testsList =
    STM.atomically $ do
        tests <- STM.readTVar testsList
        case canAddToSchedule name tests of
            notPossible@(Left _) -> return notPossible
            isPossible -> do
                updateStatus testsList name (Scheduled schedule)
                return isPossible

tryRunningNow :: Config -> Text -> ScheduleOptions -> IO ()
tryRunningNow config@(Config {..}) name ScheduleOptions {..} = do
    canRun <- STM.atomically (markAsRunning testsList name)
    guard canRun
    now <- getCurrentTime
    let steps = Wrecker.Concurrency <$> createSteps
        fTime = pack . formatISO8601 $ now
        groupName = gName <> " - " <> fTime
        runner = escalateWithRecorder config name time groupName
    job <- async (runLocalProcess localProcess $ runner steps)
    STM.atomically (updateStatus testsList name (Running $ Just job))
  where
    createSteps =
        if cStart == 1 || cStart == 0
            then tail [0,sStep .. cEnd]
            else [cStart,(cStart + sStep) .. cEnd]

escalateWithRecorder ::
       Config -- ^ The scheduler configuration
    -> Text -- ^ The test name to execute
    -> Maybe Int -- ^ Ampunt of seconds to spend on each test client
    -> Text -- ^ The group name to assign to this test run
    -> ([Wrecker.Concurrency] -> Process Wrecker.Result)
escalateWithRecorder Config {db, slaves} name time groupName =
    let secs = Wrecker.Seconds (fromMaybe 10 time)
        builder = Wrecker.Command name secs
        executer = remoteWrecker (head slaves)
        recorder c r = liftIO (Recorder.record db groupName c r)
    in Wrecker.escalate builder executer recorder

remoteWrecker :: NodeId -> Wrecker.Command -> Process (Either String WreckerRun)
remoteWrecker slave command = call $(functionTDict 'wrecker) slave ($(mkClosure 'wrecker) command)

-- | Modifies the test list if it is valid to insert the new schedule
markAsRunning :: RunSchedule -> Text -> STM.STM Bool
markAsRunning testsList name = do
    tests <- STM.readTVar testsList
    if isRunnable name tests
        then do
            updateStatus testsList name (Running Nothing)
            return True
        else return False

updateStatus :: RunSchedule -> Text -> RunStatus -> STM.STM ()
updateStatus testsList name s = STM.modifyTVar' testsList (Map.insert name s)

-- | Returns whehter it is ok to schedule or run the given test
--   Left is returned when it is not possible to run the test.
--   Otherwise the test is ok to be set for scheduling
canAddToSchedule :: Text -> Map Text RunStatus -> Either Text Bool
canAddToSchedule name list =
    case Map.lookup name list of
        Nothing -> Left "Test name does not exist"
        Just (Running _) -> Left "This test is still running"
        Just _ -> Right True

isRunnable :: Text -> Map Text RunStatus -> Bool
isRunnable name list =
    case Map.lookup name list of
        Just (Scheduled _) -> True
        _ -> False

----------------------------------
-- JSON Conversions
----------------------------------
instance ToJSON RunStatus where
    toJSON (Running _) = String "running"
    toJSON Done = String "done"
    toJSON (Scheduled _) = String "scheduled"
    toJSON None = String "none"
