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
import System.IO.Temp (withSystemTempFile)
import System.Process (callProcess, readCreateProcess, shell)

newtype Concurrency =
    Concurrency Int
    deriving (Eq, Show, Typeable, Generic)

newtype Seconds =
    Seconds Int
    deriving (Eq, Show, Typeable, Generic)

data Command = Command
    { title :: Text
    , seconds :: Seconds
    , concurrency :: Concurrency
    } deriving (Eq, Show, Generic, Typeable)

-- We need these intances to serialize commands over the network
instance Binary Concurrency

instance Binary Seconds

instance Binary Command

data Result
    = Success
    | ExecError [Text]
    | TooManyErrors
    deriving (Show)

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

wrecker :: Command -> IO (Either String WreckerRun)
wrecker (Command title (Seconds secs) (Concurrency conc)) = do
    res <- try runScript
    case res :: Either SomeException (Either String WreckerRun) of
        Left err -> return (Left (show err))
        Right runResult -> return runResult
  where
    runScript = do
        withSystemTempFile "wrecker.results" $ \file handle -> do
            callProcess
                "sg-wrecker"
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

listGroups :: IO [String]
listGroups = do
    output <- readCreateProcess (shell "sg-wrecker --list-test-groups") ""
    let each = lines output
    return (fmap (dropWhile (\c -> c == '>' || c == ' ')) each)
