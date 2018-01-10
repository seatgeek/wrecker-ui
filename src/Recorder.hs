{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Recorder where

import Data.Either (lefts, rights)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Database.Persist (insert)
import Invoker (Command(..), Concurrency(..))
import Model
       (Database, Key, Run(..), RunGroup(..), WreckerRun(..),
        findOrCreateGroupSet, runDbAction, storeRunResults, toKey)

data RunGroupOptions = RunGroupOptions
    { title :: Text
    , notes :: Text
    , concurrencyStart :: Int
    , concurrencyTarget :: Int
    , rampupStep :: Int
    , runKeepTime :: Int
    , groupSetId :: Maybe Int
    }

createRunGroup :: Database -> RunGroupOptions -> IO (Key RunGroup)
createRunGroup db RunGroupOptions {..} = do
    now <- getCurrentTime
    runDbAction db $ do
        setId <-
            case groupSetId of
                Nothing ->
                    findOrCreateGroupSet
                        "Default Set"
                        "When no group set is selected, the default one is used"
                        now
                Just gId -> return (toKey gId)
        insert
            RunGroup
            { runGroupGroupSetId = setId
            , runGroupTitle = title
            , runGroupNotes = notes
            , runGroupConcurrencyStart = concurrencyStart
            , runGroupConcurrencyTarget = concurrencyTarget
            , runGroupRampupStep = rampupStep
            , runGroupRunKeepTime = runKeepTime
            , runGroupCreated = now
            }

createRun :: Database -> Key RunGroup -> Command -> IO (Key Run)
createRun db groupId (Command title _ (Concurrency concurrency)) = do
    now <- getCurrentTime
    runDbAction db $ insert $ Run groupId title concurrency now

record :: Database -> Key Run -> [Either String WreckerRun] -> IO ()
record db runId result =
    case lefts result of
        [] -> runDbAction db $ mapM_ (storeRunResults runId) (rights result)
        errors -> do
            putStrLn "Got an error when processing wrecker results:"
            print errors
