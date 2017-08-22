{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Recorder where

import Data.Either (lefts, rights)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Database.Persist (insert)
import Invoker (Command(..), Concurrency(..))
import Model
       (Database, Key, Run(..), WreckerRun(..), runDbAction,
        storeRunResults)

createRun :: Database -> Text -> Command -> IO (Key Run)
createRun db gName (Command title _ (Concurrency concurrency)) = do
    now <- getCurrentTime
    runDbAction db $ insert $ Run title gName concurrency now

record :: Database -> Key Run -> [Either String WreckerRun] -> IO ()
record db runId result = do
    case lefts result of
        [] -> runDbAction db $ mapM_ (storeRunResults runId) (rights result)
        errors -> do
            putStrLn "Got an error when processing wrecker results:"
            print errors
