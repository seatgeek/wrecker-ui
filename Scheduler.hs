{-# LANGUAGE RecordWildCards #-}

module Scheduler
  ( RunSchedule
  , ScheduleOptions(..)
  , emptyRunSchedule
  , runScheduler
  , getRunSchedule
  , addToSchedule
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, pollSTM, Async)
import qualified Control.Concurrent.STM as STM
import Data.Aeson
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Maybe as Maybe
import Data.Text (Text, pack)
import qualified Invoker as Wrecker
import Model (Database)

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
  } deriving (Show)

type RunSchedule = STM.TVar (Map Text RunStatus)

emptyRunSchedule :: IO RunSchedule
emptyRunSchedule = do
  list <- Wrecker.listGroups
  STM.newTVarIO (Map.fromList [(pack t, None) | t <- list])

getRunSchedule :: STM.TVar a -> IO a
getRunSchedule testsList = STM.atomically (STM.readTVar testsList)

runScheduler :: Database -> RunSchedule -> IO ()
runScheduler db testsList = do
  threadDelay $ 15 * 1000 * 1000 -- wait 15 seconds
  maybeSchedule <-
    STM.atomically $ do
      tests <- STM.readTVar testsList
      cleaned <- cleanupFinished tests
      STM.writeTVar testsList cleaned
      pickOneToRun
  process maybeSchedule
  runScheduler db testsList
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
        (name, Scheduled schedule):_ -> do
          _ <- addToSchedule db name schedule testsList
          return ()
        _ -> return ()

addToSchedule :: Database -> Text -> ScheduleOptions -> RunSchedule -> IO (Either Text Bool)
addToSchedule db name schedule@ScheduleOptions {..} testsList = do
  result <- STM.atomically changeList
  case result of
    Right True -> do
      let steps = createSteps
      job <- async (Wrecker.escalate db gName name steps)
      STM.atomically (updateStatus (Running $ Just job))
      return result
    _ -> return result
  where
    createSteps =
      if cStart == 1 || cStart == 0
        then tail [0,sStep .. cEnd]
        else [cStart,(cStart + sStep) .. cEnd]
    updateStatus :: RunStatus -> STM.STM ()
    updateStatus s = STM.modifyTVar' testsList $ Map.insert name s
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
-- JSON Conversions
----------------------------------
instance ToJSON RunStatus where
  toJSON (Running _) = String "running"
  toJSON Done = String "done"
  toJSON (Scheduled _) = String "scheduled"
  toJSON None = String "none"
