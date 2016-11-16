{-# LANGUAGE OverloadedStrings #-}
module Hayoo.App.Metrics
  ( -- * Types
    Metric
  , Store -- reexport for easier usage

    -- * Operations
  , createMetric
  , collectStats
  , measureAndStore
  , currentCount
  , stats
  ) where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Data.Int                    (Int64)
import qualified Data.Text                   as T
import           Data.Time                   (diffUTCTime, getCurrentTime)
import           System.Metrics              (Store)
import           System.Metrics              (sampleAll, createCounter, createDistribution)
import           System.Metrics.Counter      (Counter)
import qualified System.Metrics.Counter      as EKGC
import           System.Metrics.Distribution (Distribution, Stats)
import qualified System.Metrics.Distribution as EKGD
import qualified System.Metrics.Json         as EKGJ


-- TYPES

data Metric = Metric
  { counter      :: Counter
  , distribution :: Distribution
  }


-- OPERATIONS


createMetric :: (MonadIO m) => T.Text -> T.Text -> Store -> m Metric
createMetric counterName statName store = do
  counter <- liftIO $ createCounter counterName store
  stats   <- liftIO $ createDistribution statName store
  return $ Metric counter stats


currentCount :: (MonadIO m) => Metric -> m Int64
currentCount = liftIO . EKGC.read . counter


stats :: (MonadIO m) => Metric -> m Stats
stats = liftIO . EKGD.read . distribution


measureAndStore :: (MonadIO m) => m a -> Metric -> m a
measureAndStore action metric = do
  liftIO $ EKGC.inc $ counter metric -- Note: it's intended to count regardless of errors
  (result, timeDelta) <- measureExecTime action
  liftIO $ EKGD.add (distribution metric) timeDelta
  return result


measureExecTime :: (MonadIO m) => m a -> m (a, Double)
measureExecTime action = do
  t0 <- liftIO getCurrentTime
  r  <- action
  t1 <- liftIO getCurrentTime
  let delta = t1 `diffUTCTime` t0
  return (r, (realToFrac delta))


collectStats :: (MonadIO m) => Store -> m EKGJ.Sample
collectStats store = do
  result <- liftIO $ sampleAll store
  return $ EKGJ.Sample result
