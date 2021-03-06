{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger
       (LoggingT(..), filterLogger, runStdoutLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Resource as R
import Data.Aeson hiding (Value)
import Data.Aeson.Types (Options(..), Parser)
import Data.Binary (Binary(..))
import Data.Char (toLower)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Maybe
import Data.Monoid ((<>))
import Data.Pool (Pool)
import Data.Ratio (numerator)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Typeable (Typeable)
import Database.Esqueleto
import Database.Persist (Key(..))
import qualified Database.Persist as P
import Database.Persist.Postgresql (withPostgresqlPool)
import qualified Database.Persist.Sql as Sql
import Database.Persist.Sqlite (withSqlitePool)
import Database.Persist.TH
import Database.PostgreSQL.Simple.Internal
       (ConnectInfo, postgreSQLConnectionString)
import GHC.Generics hiding (from)

-------------------------------------
-- Schema Definition
-------------------------------------
share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|

  GroupSet
    name Text
    description Text
    created UTCTime
    deriving Eq Show Generic

  RunGroup
    groupSetId GroupSetId
    title Text
    notes Text
    concurrencyStart Int
    concurrencyTarget Int
    rampupStep Int
    runKeepTime Int
    created UTCTime
    deriving Eq Show Generic

  Run
    runGroupId RunGroupId
    match Text
    concurrency Int
    created UTCTime
    deriving Eq Show Generic

  Rollup
    runId RunId Maybe
    hits Int
    maxTime Double
    meanTime Double
    minTime Double
    totalTime Double
    variance Double
    successHits Int
    userErrorHits Int
    serverErrorHits Int
    failedHits Int
    quantile95 Double
    deriving Eq Show Generic Typeable

  Page
    runId RunId Maybe
    url Text Maybe
    hits Int
    maxTime Double
    meanTime Double
    minTime Double
    totalTime Double
    variance Double
    successHits Int
    userErrorHits Int
    serverErrorHits Int
    failedHits Int
    quantile95 Double
    deriving Eq Show Generic Typeable
|]

----------------------------------
-- Convenient type aliases
----------------------------------
type DbMonad m = (MonadBaseControl IO m, MonadIO m)

type TransactionMonad m a = ReaderT SqlBackend (R.ResourceT m) a

type Database = Pool SqlBackend

type DbAction m = ReaderT SqlBackend m

data DbBackend
    = Postgresql ConnectInfo
    | Sqlite

data LogLevel
    = Silent
    | Debug
    deriving (Show, Read, Eq)

data WreckerRun = WreckerRun
    { rollup :: Rollup
    , pages :: [Page]
    } deriving (Eq, Show, Generic, Typeable)

instance Binary WreckerRun

instance Binary Rollup

instance Binary Page

instance Binary (Key Run) where
    get = fmap Sql.toSqlKey Data.Binary.get
    put = put . Sql.fromSqlKey

instance Binary (Key Page) where
    get = fmap Sql.toSqlKey Data.Binary.get
    put = put . Sql.fromSqlKey

-------------------------------------
-- Functions for working with the DB
-------------------------------------
withDb :: DbMonad m => LogLevel -> DbBackend -> (Database -> LoggingT m a) -> m a
withDb logLevel dbType =
    case logLevel of
        Debug -> runStdoutLoggingT . withPool 10
        Silent -> runStdoutLoggingT . filterLogger (\_ _ -> False) . withPool 10
  where
    withPool size =
        case dbType of
            Sqlite -> withSqlitePool "wrecker.db" size
            Postgresql info -> withPostgresqlPool (postgreSQLConnectionString info) size

runDbAction :: DbMonad m => Database -> TransactionMonad m a -> m a
runDbAction db = R.runResourceT . flip Sql.runSqlPool db

runMigrations :: DbMonad m => Database -> m ()
runMigrations db = runDbAction db (Sql.runMigration migrateAll)

-------------------------------------
-- Finder queries
-------------------------------------
findGroupSetsByName :: MonadIO m => Text -> SqlReadT m [Entity GroupSet]
findGroupSetsByName match =
    let matchWithExpanse = val ("%" <> match <> "%")
    in select $
       from $ \gs -> do
           where_ (gs ^. GroupSetName `like` matchWithExpanse)
           return gs

findGroupSetsByRunMatch :: MonadIO m => Text -> SqlReadT m [Entity GroupSet]
findGroupSetsByRunMatch match =
    let matchWithExpanse = val ("%" <> match <> "%")
    in select $
       distinct $
       from $ \(r, rg, gs) -> do
           where_ (r ^. RunRunGroupId ==. rg ^. RunGroupId)
           where_ (rg ^. RunGroupGroupSetId ==. gs ^. GroupSetId)
           where_ (r ^. RunMatch `like` matchWithExpanse)
           return gs

findRunsByMatchAndSet ::
       MonadIO m
    => Text -- ^ The test title
    -> Maybe Int -- ^ Optionally the groupset id to filter on
    -> Maybe Int -- ^ Optionally limit the number of distinct run gorups
    -> SqlReadT m [Entity Run]
findRunsByMatchAndSet match groupSetId runGroupLimit =
    select $
    from $ \r -> do
        where_ (r ^. RunRunGroupId `in_` subList_select countFilter)
        return r
  where
    matchFilter =
        let matchWithExpanse = val ("%" <> match <> "%")
        in from $ \r -> do
               where_ (r ^. RunMatch `like` matchWithExpanse)
               return $ r ^. RunRunGroupId
    countFilter =
        from $ \rg -> do
            where_ (rg ^. RunGroupId `in_` subList_select matchFilter)
            unless (Data.Maybe.isNothing groupSetId) $
                where_
                    (rg ^. RunGroupGroupSetId ==. val (toKey $ Data.Maybe.fromMaybe 0 groupSetId))
            orderBy [desc (rg ^. RunGroupCreated)]
            unless (Data.Maybe.isNothing runGroupLimit) $
                limit $ fromIntegral (Data.Maybe.fromMaybe 1 runGroupLimit)
            return $ rg ^. RunGroupId

findRuns :: MonadIO m => [Key Run] -> SqlReadT m [Entity Run]
findRuns keys =
    select $
    from $ \r -> do
        where_ (r ^. RunId `in_` valList keys)
        orderBy [asc (r ^. RunCreated)]
        return r

findRunGroups :: MonadIO m => [Key Run] -> SqlReadT m [Entity RunGroup]
findRunGroups keys =
    select $
    from $ \r -> do
        where_ (r ^. RunGroupId `in_` subList_select findRunGroupIds)
        orderBy [asc (r ^. RunGroupCreated)]
        return r
  where
    findRunGroupIds =
        from $ \r -> do
            where_ (r ^. RunId `in_` valList keys)
            orderBy [asc (r ^. RunCreated)]
            return (r ^. RunRunGroupId)

findPagesList :: MonadIO m => [Key Run] -> SqlReadT m [(Key Run, Text)]
findPagesList runIds = do
    rows <-
        select $
        distinct $
        from $ \p -> do
            where_ (p ^. PageRunId `in_` justList (valList runIds))
            return
                ( coalesceDefault [p ^. PageRunId] (val (toKey 0))
                , coalesceDefault [p ^. PageUrl] (val ""))
    return (fmap (\(k, p) -> (unValue k, unValue p)) rows)

findRunStats :: MonadIO m => [Key Run] -> SqlReadT m [(Key Run, Rollup)]
findRunStats runIds = do
    rows <-
        select $
        from $ \rollup -> do
            where_ (rollup ^. RollupRunId `in_` justList (valList (runIds)))
            groupBy (rollup ^. RollupRunId)
            orderBy [asc (rollup ^. RollupRunId)]
            return
                ( coalesceDefault [rollup ^. RollupRunId] (val (toKey 0))
                , coalesceDefault [sum_ (rollup ^. RollupHits)] (val 0)
                , coalesceDefault [max_ (rollup ^. RollupMaxTime)] (val 0)
                , coalesceDefault [avg_ (rollup ^. RollupMeanTime)] (val 0)
                , coalesceDefault [min_ (rollup ^. RollupMinTime)] (val 0)
                , coalesceDefault [sum_ (rollup ^. RollupTotalTime)] (val 0)
                , coalesceDefault [avg_ (rollup ^. RollupVariance)] (val 0)
                , coalesceDefault [sum_ (rollup ^. RollupSuccessHits)] (val 0)
                , coalesceDefault [sum_ (rollup ^. RollupUserErrorHits)] (val 0)
                , coalesceDefault [sum_ (rollup ^. RollupServerErrorHits)] (val 0)
                , coalesceDefault [sum_ (rollup ^. RollupFailedHits)] (val 0)
                , coalesceDefault [avg_ (rollup ^. RollupQuantile95)] (val 0))
    return (fmap buildRollup rows)
  where
    buildRollup fields = set11Fields (Rollup Nothing) fields

findPageStats :: MonadIO m => [Key Run] -> Text -> SqlReadT m [Page]
findPageStats runIds url = do
    rows <-
        select $
        from $ \page -> do
            where_ (page ^. PageRunId `in_` justList (valList runIds))
            where_ (page ^. PageUrl ==. just (val url))
            groupBy (page ^. PageRunId)
            return
                ( coalesceDefault [page ^. PageRunId] (val (toKey 0))
                , coalesceDefault [sum_ (page ^. PageHits)] (val 0)
                , coalesceDefault [max_ (page ^. PageMaxTime)] (val 0)
                , coalesceDefault [avg_ (page ^. PageMeanTime)] (val 0)
                , coalesceDefault [min_ (page ^. PageMinTime)] (val 0)
                , coalesceDefault [sum_ (page ^. PageTotalTime)] (val 0)
                , coalesceDefault [avg_ (page ^. PageVariance)] (val 0)
                , coalesceDefault [sum_ (page ^. PageSuccessHits)] (val 0)
                , coalesceDefault [sum_ (page ^. PageUserErrorHits)] (val 0)
                , coalesceDefault [sum_ (page ^. PageServerErrorHits)] (val 0)
                , coalesceDefault [sum_ (page ^. PageFailedHits)] (val 0)
                , coalesceDefault [avg_ (page ^. PageQuantile95)] (val 0))
    return (fmap buildPage rows)
  where
    buildPage fields = setRunId . buildIntermediate $ fields
    buildIntermediate fields = set11Fields (Page Nothing (Just url)) fields
    setRunId (rId, page) = page {pageRunId = Just rId}

toKey :: ToBackendKey SqlBackend a => Int -> Key a
toKey = Sql.toSqlKey . fromIntegral

fromKey :: ToBackendKey SqlBackend a => Key a -> Int
fromKey = fromIntegral . Sql.fromSqlKey

set11Fields ::
       (Int -> t7 -> t6 -> t5 -> t4 -> t3 -> Int -> Int -> Int -> Int -> t2 -> t1)
    -> ( Value t
       , Value Rational
       , Value t7
       , Value t6
       , Value t5
       , Value t4
       , Value t3
       , Value Rational
       , Value Rational
       , Value Rational
       , Value Rational
       , Value t2)
    -> (t, t1)
set11Fields func (Value key, Value a, Value b, Value c, Value d, Value e, Value f, Value g, Value h, Value i, Value j, Value k) =
    ( key
    , func -- Some fields need casting
          (castInt a)
          b
          c
          d
          e
          f
          (castInt g)
          (castInt h)
          (castInt i)
          (castInt j)
          k)
  where
    castInt :: Rational -> Int
    castInt = fromIntegral . numerator

storeRunResults :: DbMonad m => Key Run -> WreckerRun -> DbAction m ()
storeRunResults runKey WreckerRun {..} = do
    let fullRollup = rollup {rollupRunId = Just runKey}
        fullPage page = page {pageRunId = Just runKey}
  --
  -- We insert both the rollup and the pages in the same transaction
    _ <- P.insert fullRollup
    mapM_ (P.insert . fullPage) pages

findOrCreateGroupSet :: DbMonad m => Text -> Text -> UTCTime -> DbAction m (Key GroupSet)
findOrCreateGroupSet setName setDescription now = do
    groupSet <-
        select $
        from $ \g -> do
            where_ (g ^. GroupSetName ==. val setName)
            limit 1
            return g
    if null groupSet
        then insert $ GroupSet setName setDescription now
        else let gs:_ = groupSet
             in return (entityKey gs)

-------------------------------------
-- JSON conversions
-------------------------------------
instance ToJSON GroupSet where
    toJSON = genericToJSON (defaultOptions {fieldLabelModifier = lcFirst . drop 8})

instance ToJSON RunGroup where
    toJSON = genericToJSON (defaultOptions {fieldLabelModifier = lcFirst . drop 8})

instance ToJSON Run where
    toJSON = genericToJSON (defaultOptions {fieldLabelModifier = lcFirst . drop 3})

instance FromJSON Run where
    parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = lcFirst . drop 3})

instance ToJSON Rollup where
    toJSON = genericToJSON (defaultOptions {fieldLabelModifier = lcFirst . drop 6})

instance ToJSON Page where
    toJSON = genericToJSON (defaultOptions {fieldLabelModifier = lcFirst . drop 4})

lcFirst :: String -> String
lcFirst "" = ""
lcFirst (f:rest) = toLower f : rest

instance FromJSON Rollup where
    parseJSON =
        withObject "Rollup" $ \o -> do
            let rollupRunId = Nothing
            rollup <- o .: "rollup"
            rollupMaxTime <- rollup .: "max"
            rollupMeanTime <- rollup .: "mean"
            rollupMinTime <- rollup .: "min"
            rollupHits <- rollup .: "count"
            rollupVariance <- rollup .: "variance"
            rollupTotalTime <- rollup .: "total"
            rollupQuantile95 <- rollup .: "quantile95"
            successData <- o .: "2xx"
            rollupSuccessHits <- successData .: "count"
            userErrorData <- o .: "4xx"
            rollupUserErrorHits <- userErrorData .: "count"
            serverErrorData <- o .: "5xx"
            rollupServerErrorHits <- serverErrorData .: "count"
            failureData <- o .: "failed"
            rollupFailedHits <- failureData .: "count"
            return Rollup {..}

instance FromJSON Page where
    parseJSON =
        withObject "Page" $ \o -> do
            let pageRunId = Nothing
            let pageUrl = Nothing
            rollup <- o .: "rollup"
            pageMaxTime <- rollup .: "max"
            pageMeanTime <- rollup .: "mean"
            pageMinTime <- rollup .: "min"
            pageHits <- rollup .: "count"
            pageVariance <- rollup .: "variance"
            pageTotalTime <- rollup .: "total"
            pageQuantile95 <- rollup .: "quantile95"
            successData <- o .: "2xx"
            pageSuccessHits <- successData .: "count"
            userErrorData <- o .: "4xx"
            pageUserErrorHits <- userErrorData .: "count"
            serverErrorData <- o .: "5xx"
            pageServerErrorHits <- serverErrorData .: "count"
            failureData <- o .: "failed"
            pageFailedHits <- failureData .: "count"
            return Page {..}

instance FromJSON WreckerRun where
    parseJSON =
        withObject "WreckerRun" $ \o -> do
            rollup <- o .: "rollup"
            allPages <- o .: "per-request" :: Parser (Map Text Page)
            -- Now we convert the pages dictionary into a list of 'Page'
            -- by traversing all the structure with a accumulator function
            let pages =
                    Map.foldlWithKey'
                        (\list url page -> page {pageUrl = Just url} : list)
                        [] -- The initial accumulator value
                        allPages
            return WreckerRun {..}

instance ToJSON a => ToJSON (Entity a) where
    toJSON (Entity k run) =
        let (Object encoded) = toJSON run -- first encode the run
            (Object eKey) = object ["id" .= k] -- convert the key to json
        in Object (encoded <> eKey) -- Add both parts together as an object
