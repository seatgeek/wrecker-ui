{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Invoker where

import Control.Exception (try, SomeException)
import Control.Monad.Loops (andM)
import Data.Aeson (eitherDecodeStrict)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, unpack, pack)
import Data.Time.Clock (getCurrentTime)
import Data.Time.ISO8601 (formatISO8601)
import Database.Persist (insert)
import Model
       (Database, WreckerRun(..), Run(..), Rollup(..), runDbAction,
        storeRunResults)
import System.IO.Temp (withSystemTempFile)
import System.Process (callProcess, shell, readCreateProcess)

newtype Concurrency =
  Concurrency Int

newtype Seconds =
  Seconds Int

escalateAll :: Database -> Text -> IO ()
escalateAll db gName = do
  titles <- listGroups
  now <- getCurrentTime
  let jobs = [(pack t, Concurrency c) | t <- titles, c <- tail [0,10 .. 300]]
      fTime = pack . formatISO8601 $ now
      groupName = gName <> " - " <> fTime
      runner = record db groupName
  _ <- andM $ fmap (\(title, conc) -> runner title Nothing conc) jobs
  return ()

escalate :: Database -> Text -> Text -> Maybe Seconds -> [Concurrency] -> IO ()
escalate db gName title seconds steps = do
  now <- getCurrentTime
  let fTime = pack . formatISO8601 $ now
      groupName = gName <> " - " <> fTime
      runner = record db groupName title seconds
  _ <- andM (fmap runner steps)
  return ()

record :: Database -> Text -> Text -> Maybe Seconds -> Concurrency -> IO Bool
record db gName title seconds conc@(Concurrency concurrency) = do
  now <- getCurrentTime
  let secs = fromMaybe (Seconds 10) seconds
  result <- wrecker title conc secs
  case result of
    Left err -> do
      putStrLn "Got an error when processing wrecker results:"
      putStrLn err
      return False
    Right run ->
      if isUnAcceptable run
        then return False
        else runDbAction db $ do
               runId <- insert $ Run title gName concurrency now
               storeRunResults runId run
               return True
  where
    isUnAcceptable :: WreckerRun -> Bool
    -- | A run is not acceptable if more than 1% of the hits were errors
    isUnAcceptable WreckerRun {rollup = Rollup {..}} =
      (fromIntegral (rollupUserErrorHits + rollupServerErrorHits + rollupFailedHits)) /
      (fromIntegral rollupHits) >
      (0.01 :: Double)

wrecker :: Text -> Concurrency -> Seconds -> IO (Either String WreckerRun)
wrecker title (Concurrency conc) (Seconds secs) = do
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
