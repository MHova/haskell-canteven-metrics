{-# LANGUAGE NamedFieldPuns  #-}

-- | Provides a way to easily setup metrics using EKG.
--
-- It uses @canteven-config@ to obtain a configuration used to setup @ekg@
-- server and @ekg-carbon@.
module Canteven.Metrics (
  setupMetrics,
  add
) where

import Control.Monad (void)
import Canteven.Metrics.Distribution (add)
import Canteven.Metrics.Types (MetricsConfig(MetricsConfig, ekgHost, ekgPort,
  carbon), CarbonConfig(CarbonConfig, carbonOptions))
import Data.ByteString.Char8 (pack)
import System.Metrics (Store)
import System.Remote.Monitoring (forkServer, serverMetricStore)
import System.Remote.Monitoring.Carbon (forkCarbon)

import qualified Canteven.Config as Config (canteven)


-- | Read configuration, fork EKG server, and flush metrics to Carbon
setupMetrics :: IO ()
setupMetrics = do
  MetricsConfig {ekgHost, ekgPort, carbon} <- Config.canteven
  handle <- forkServer (pack ekgHost) ekgPort
  let store = serverMetricStore handle
  flushMetricsToCarbon carbon store


-- | Fork a thread to flush EKG metrics to Graphite.
flushMetricsToCarbon :: Maybe CarbonConfig -> Store -> IO ()
flushMetricsToCarbon Nothing _ = return ()
flushMetricsToCarbon (Just CarbonConfig {carbonOptions}) store =
  void $ forkCarbon carbonOptions store
