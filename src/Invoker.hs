{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Invoker where

import Control.Exception (SomeException, try)
import Control.Monad (foldM)
import Data.Aeson (eitherDecodeStrict)
import Data.Binary (Binary)
import qualified Data.ByteString as BS
import Data.Either (lefts, rights)
import Data.Text (Text, pack, unpack)
import Data.Typeable (Typeable)
import GHC.Generics
import Model (Rollup(..), WreckerRun(..))
import System.Environment (lookupEnv)
import System.IO.Temp (withSystemTempFile)
import System.Process (callProcess, readCreateProcess, shell)

-- | Represents the concurrency (number of threads) used to execute a run
newtype Concurrency =
    Concurrency Int
    deriving (Eq, Show, Typeable, Generic)

-- | The number of seconds to spend on a particular run.
newtype Seconds =
    Seconds Int
    deriving (Eq, Show, Typeable, Generic)

-- | Specifies a wrecker CLI command
data Command = Command
    { title :: Text
    , seconds :: Seconds
    , concurrency :: Concurrency
    } deriving (Eq, Show, Generic, Typeable)

-- We need these intances to serialize commands over the network
instance Binary Concurrency

instance Binary Seconds

instance Binary Command

-- | Represents the exit status of a wrecker invocation.
data Result
    = Success
    | ExecError [Text]
    | TooManyErrors
    deriving (Show)

-- | A helper function used to execute a group of wrecker invocations, given a list of concurrency levels.
--   This function is higly polymorphic, it only encodes the idea of repeatedly building a command, executing
--   it and recording its results. You have to provide the implementation of each of those individual steps
escalate ::
       Monad m
    => (Concurrency -> Command) -- ^ A function that given the concurrecy, will create a Command record
    -> (Command -> m [Either String WreckerRun]) -- ^ A function to execute the tests in wrecker
    -> (Command -> m key) -- ^ A function to create the run identifier
    -> (key -> [Either String WreckerRun] -> m ()) -- ^ A function to record the run
    -> [Concurrency] -- ^ The list of concurrency steps to execute in wrecker
    -> m Result
escalate builder executer keyGen recorder steps = foldM run Success steps
  where
    run prev step = do
        case prev of
            Success -> execution step
            err -> return err
    execution step = do
        let command = builder step
        key <- keyGen command
        results <- executer command
        recorder key results
        case lefts results of
            [] ->
                if isUnAcceptable (rights results)
                    then return TooManyErrors
                    else return Success
            errors -> return (ExecError (fmap pack errors))

-- | Whether or not there were too many errors when invoking a wrecker run
isUnAcceptable :: [WreckerRun] -> Bool
isUnAcceptable [] = False
-- | A run is not acceptable if more than 1% of the hits were errors
isUnAcceptable results =
    let (errors, hits) = foldr extract (0, 0) results
    in errors / hits > (0.01 :: Double)
  where
    extract (WreckerRun {rollup = Rollup {..}}) (errors, hits) =
        ( errors + fromIntegral (rollupUserErrorHits + rollupServerErrorHits + rollupFailedHits)
        , hits + fromIntegral rollupHits)

-- | Invokes the wrecker command line tool with the given options
wrecker :: Command -> IO (Either String WreckerRun)
wrecker (Command title (Seconds secs) (Concurrency conc)) = do
    res <- try runScript
    case res :: Either SomeException (Either String WreckerRun) of
        Left err -> return (Left (show err))
        Right runResult -> return runResult
  where
    runScript = do
        executable <- findExecutable
        withSystemTempFile "wrecker.results" $ \file handle -> do
            callProcess
                executable
                [ "--concurrency"
                , show conc
                , "--log-level"
                , "LevelInfo"
                , "--match"
                , unpack title
                , "--run-timed"
                , show secs
                , "--output-path"
                , file
                ]
            contents <- BS.hGetContents handle
            return (eitherDecodeStrict contents)

-- | Returns the list of tests that are executable in the wrecker CLI
listGroups :: IO [String]
listGroups = do
    executable <- findExecutable
    output <- readCreateProcess (shell $ executable ++ " --list-test-groups") ""
    let each = lines output
    return (fmap (dropWhile (\c -> c `elem` notMeaningfulChars)) each)
  where
    notMeaningfulChars = '.' : '>' : ' ' : ['0' .. '9']

-- | Returns the default executable name or the one present in the WRECKER_EXECUTABLE env var
findExecutable :: IO String
findExecutable = do
    executable <- lookupEnv "WRECKER_EXECUTABLE"
    return (maybe "wrecker" id executable)
