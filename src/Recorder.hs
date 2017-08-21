{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Recorder where

import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Database.Persist (insert)
import Invoker (Command(..), Concurrency(..))
import Model
       (Database, Run(..), WreckerRun(..), runDbAction, storeRunResults)

record :: Database -> Text -> Command -> Either String WreckerRun -> IO ()
record db gName (Command title _ (Concurrency concurrency)) result = do
    now <- getCurrentTime
    case result of
        Left err -> do
            putStrLn "Got an error when processing wrecker results:"
            putStrLn err
        Right run ->
            runDbAction db $ do
                runId <- insert $ Run title gName concurrency now
                storeRunResults runId run
