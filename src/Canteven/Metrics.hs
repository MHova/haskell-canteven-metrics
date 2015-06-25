{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns  #-}

-- | Provides a way to easily setup metrics using EKG.
--
-- It uses @canteven-config@ to obtain a configuration used to setup @ekg@
-- server and @ekg-carbon@.
module Canteven.Metrics (
  setupMetrics,
  sample
) where

import Control.Monad (void)
import Canteven.Metrics.Distribution (sample)
import Canteven.Metrics.Types (MetricsConfig(MetricsConfig, ekgHost, ekgPort,
  carbon), CarbonConfig(CarbonConfig, carbonOptions))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Text (append)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.HTTP.Client (Manager)
import System.Metrics (Store)
import System.Remote.Monitoring (forkServer, serverMetricStore)
import System.Remote.Monitoring.Carbon (forkCarbon,
  CarbonOptions(CarbonOptions, prefix))

import qualified Canteven.Config as Config (canteven)
import qualified Canteven.Metrics.Aws as Aws (instanceId)


-- | Read configuration, fork EKG server, and flush metrics to Carbon.
--
-- It uses EC2 instance's id to compose Carbon metrics prefix, useful to
-- distinguish between metrics from different instances of the same app.
setupMetrics :: Manager -> IO Store
setupMetrics manager = do
  MetricsConfig {ekgHost, ekgPort, carbon} <- Config.canteven
  handle <- forkServer (pack ekgHost) ekgPort
  let store = serverMetricStore handle
  instanceId <- Aws.instanceId manager
  flushMetricsToCarbon carbon store instanceId
  return store


-- | Fork a thread to flush EKG metrics to Graphite.
flushMetricsToCarbon :: Maybe CarbonConfig -> Store -> ByteString -> IO ()
flushMetricsToCarbon Nothing _ _ = return ()
flushMetricsToCarbon (Just CarbonConfig {carbonOptions}) store instanceId =
  void $ forkCarbon (appendInstanceId instanceId carbonOptions) store


-- | Append EC2 instance's id to Carbon metrics prefix.
appendInstanceId :: ByteString -> CarbonOptions -> CarbonOptions
appendInstanceId "" options = options
appendInstanceId instanceId options@CarbonOptions {prefix} =
    options {prefix = newPrefix}
  where
    newPrefix = foldr append prefix [".", toStrict . decodeUtf8 $ instanceId]
