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
import Data.List.NonEmpty (NonEmpty(..), (!!), fromList)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Maybe as Maybe
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Invoker as Wrecker
import Model (Database, Key, RunGroup, WreckerRun)
import Prelude hiding ((!!))
import qualified Recorder

import Control.Distributed.Process
       (NodeId, Process, getSelfNode, liftIO)
import Control.Distributed.Process.Async
       (AsyncResult(..), AsyncTask, asyncLinked, remoteTask, wait)
import qualified Control.Distributed.Process.Async as DAsync
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Internal.Types
       (LocalProcess, runLocalProcess)

-- | Represents each of the stages a run can be at
data RunStatus
    = Running (Maybe (Async Wrecker.Result))
    | Done
    | Scheduled ScheduleOptions
    | None

-- | Avilable options for executing a run
data ScheduleOptions = ScheduleOptions
    { gName :: Text -- ^ The run group name, used to tag many similar runs with different concurrency
    , cStart :: Int -- ^ Concurrency start
    , cEnd :: Int -- ^ Concurrency end
    , sStep :: Int -- ^ Step size. This is the increment to use to get from start concurrency to end
    , time :: Maybe Int -- ^ The amount of time to spend on each of the individual runs
    , notes :: Text -- ^ A description for the run group
    , groupSetId :: Maybe Int -- ^ The group set the run will be attached to
    } deriving (Show)

-- | A transactional memory variable containing the status fo the current run and the schedule for future runs
type RunSchedule = STM.TVar (Map Text RunStatus)

-- | Scheduler configuration
data Config = Config
    { db :: Database -- ^ A database instance, used to store the run results
    , testsList :: RunSchedule -- ^ The varibale to store the current run and schedule for future runs
    , localProcess :: LocalProcess -- ^ This node description
    , slaves :: [NodeId] -- ^ A list of worker slaves used to distribute runs accross them
    }

-- | Wraps the wrecker function into the Process monad
--   This is used for remotely executing wrecker in other nodes
wrecker :: Wrecker.Command -> Process (Either String WreckerRun)
wrecker = liftIO . Wrecker.wrecker

-- Make the wrecker function ready to be called over the wire on slave nodes
remotable ['wrecker]

emptyRunSchedule :: IO RunSchedule
emptyRunSchedule = do
    list <- Wrecker.listGroups
    STM.newTVarIO (Map.fromList [(pack t, None) | t <- list])

getRunSchedule :: STM.TVar a -> IO a
getRunSchedule testsList = STM.atomically (STM.readTVar testsList)

-- | A blocking functions that refreshes the schedule state each 5 seconds.
--   This functions is responsible for executing the run jobs as they become available
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

-- | Adds a new run job to the queue. This function will wither queue the job
--   or return a reason for not being able to do so.
addToQueue :: Text -> ScheduleOptions -> RunSchedule -> IO (Either Text Bool)
addToQueue name schedule testsList =
    STM.atomically $ do
        tests <- STM.readTVar testsList
        case canAddToSchedule name tests of
            notPossible@(Left _) -> return notPossible
            isPossible -> do
                updateStatus testsList name (Scheduled schedule)
                return isPossible

-- | Checks wheter or not it is possible to executing the give run right now and
-- if possible, it executes it immediately.
tryRunningNow :: Config -> Text -> ScheduleOptions -> IO ()
tryRunningNow config@Config {..} name ScheduleOptions {..} = do
    let secs = Wrecker.Seconds (fromMaybe 10 time) -- Run for 10 seconds if no preference was given
    canRun <- STM.atomically (markAsRunning testsList name)
    guard canRun
    groupId <- liftIO $ Recorder.createRunGroup db (runGroupOptions secs)
    let steps = Wrecker.Concurrency <$> createSteps
        runner = escalateWithRecorder config name groupId secs
    job <- async (runLocalProcess localProcess $ runner steps)
    STM.atomically (updateStatus testsList name (Running $ Just job))
  where
    runGroupOptions (Wrecker.Seconds secs) =
        Recorder.RunGroupOptions gName notes cStart cEnd sStep secs groupSetId
    createSteps =
        if cStart == 1 || cStart == 0
            then tail [0,sStep .. cEnd]
            else [cStart,(cStart + sStep) .. cEnd]

-- | A helper function used to execute a batch of run from a starting to an ending concurrency level,
--   this helper function will try to execute wrecker remotely if the right conditions are given, and
--   will also record the results in the database.
escalateWithRecorder ::
       Config -- ^ The scheduler configuration
    -> Text -- ^ The test name to execute
    -> Key RunGroup
    -> Wrecker.Seconds
    -> ([Wrecker.Concurrency] -> Process Wrecker.Result)
escalateWithRecorder Config {db, slaves} name groupId secs =
    Wrecker.escalate (builder secs) executer keyGen recorder
  where
    builder = Wrecker.Command name
    keyGen command = liftIO (Recorder.createRun db groupId command)
    recorder key result = liftIO (Recorder.record db key result)
    executer = remoteWrecker slaves

-- | Executes a wrecker command in the remote sleves, if any are provided. The local node will always
--   participate in executing some of the load. If no slaves are available, the local node executes the whole run.
remoteWrecker :: [NodeId] -> Wrecker.Command -> Process [Either String WreckerRun]
remoteWrecker slaves command@Wrecker.Command {concurrency} = do
    thisNode <- getSelfNode
    tasks <- mapM asyncLinked (divideWork (fromList $ thisNode : slaves)) -- Execute each task remotely and async
    mapM waitFor tasks
  where
    divideWork :: NonEmpty NodeId -> [AsyncTask (Either String WreckerRun)]
    -- Try to comfortably accommodate as much work on a single node as possible
    -- and return a list of async tasks that should be executed on the network
    divideWork nodes@(selfNode :| _) =
        let Wrecker.Concurrency conc_ = concurrency
            conc = fromIntegral conc_
        in if null slaves || conc < 20
               then [buildTask command selfNode]
               else let (first:rest) =
                            [ nodes !! (i - 1) -- Return the node at position i
                            | i <- [1 .. length nodes] -- Select one more node
                            , (conc / fromIntegral i) >= 10 -- If there are at least 10 threads to run on it
                            ]
                        -- Calculate the number of threads per task to execute
                        total = fromIntegral (length (first : rest)) -- Get the total available workers
                        fairDivision = fromRational (conc / total) :: Double -- Divide the number of threads equally
                        avgConcurrency = floor fairDivision -- Round to integer
                        remainderConcurrency =
                            avgConcurrency + -- One of the workers need to get the remainder threads
                            (ceiling (total * ((conc / total) - fromIntegral avgConcurrency)))
                        setConcurrency c = command {Wrecker.concurrency = Wrecker.Concurrency c}
                        --
                        -- Build the async tasks that should be executed
                        firstTask = buildTask (setConcurrency remainderConcurrency) first -- First task gets the remainder
                        restTasks = fmap (buildTask (setConcurrency avgConcurrency)) rest -- But the rest get the same amount of threads
                    in firstTask : restTasks

-- | Builds the async task for the givend node and command
--   the "task" is executing a wrecker command on a remote machine
buildTask :: Wrecker.Command -> NodeId -> AsyncTask (Either String WreckerRun)
buildTask command node = remoteTask $(functionTDict 'wrecker) node ($(mkClosure 'wrecker) command)

-- | Waits for a remote Asyn job to finish, and returns its result once it dies or it's done.
waitFor :: DAsync.Async (Either String WreckerRun) -> Process (Either String WreckerRun)
waitFor task = do
    asyncResult <- wait task -- Wait for the task to be done
    case asyncResult of
        AsyncDone res -> return res
        AsyncFailed reason -> return (Left (show reason))
        AsyncLinkFailed reason -> return (Left (show reason))
        _ -> return (Left "Error spawning async wrecker task")

-- | Modifies the test list if it is valid to insert the new schedule
markAsRunning :: RunSchedule -> Text -> STM.STM Bool
markAsRunning testsList name = do
    tests <- STM.readTVar testsList
    if isRunnable name tests
        then do
            updateStatus testsList name (Running Nothing)
            return True
        else return False

-- | Updates the status of a single run in the give run schedule
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

-- | Checks whether or not a particular run (given its name) can be added to the queue.
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
