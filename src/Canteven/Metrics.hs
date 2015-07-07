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

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException)
import Control.Monad (void)
import Canteven.Metrics.Distribution (sample)
import Canteven.Metrics.Types (MetricsConfig(MetricsConfig, ekgHost, ekgPort,
  carbon), CarbonConfig(CarbonConfig, carbonOptions))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.HTTP.Client (Manager)
import System.IO (stderr, hPutStrLn)
import System.Metrics (Store)
import System.Remote.Monitoring (forkServer, serverMetricStore)
import System.Remote.Monitoring.Carbon (forkCarbonRestart,
  CarbonOptions(CarbonOptions, prefix))

import qualified Canteven.Config as Config (canteven)
import qualified Canteven.Metrics.Aws as Aws (instanceId)
import qualified Data.Text as T (concat)


-- | Read configuration, fork EKG server, and flush metrics to Carbon.
--
-- It uses EC2 instance's id to compose Carbon metrics prefix, useful to
-- distinguish between metrics from different instances of the same app.
setupMetrics :: Manager -> IO Store
setupMetrics manager = do
  MetricsConfig {ekgHost, ekgPort, carbon} <- Config.canteven
  handle <- forkServer (pack ekgHost) ekgPort
  let store = serverMetricStore handle
  flushMetricsToCarbon carbon store manager
  return store


-- | Fork a thread to flush EKG metrics to Graphite.
flushMetricsToCarbon :: Maybe CarbonConfig -> Store -> Manager -> IO ()
flushMetricsToCarbon Nothing _ _ = return ()
flushMetricsToCarbon (Just CarbonConfig {carbonOptions}) store manager = do
    instanceId <- Aws.instanceId manager
    let options = appendInstanceId instanceId carbonOptions
    void $ forkCarbonRestart options store err
  where
    err :: SomeException -> IO () -> IO ()
    err e restart = do
      hPutStrLn stderr $ "canteven-metrics: " ++ show e
      threadDelay 1000000
      restart


-- | Append EC2 instance's id to Carbon metrics prefix.
appendInstanceId :: ByteString -> CarbonOptions -> CarbonOptions
appendInstanceId "" options = options
appendInstanceId instanceId options@CarbonOptions {prefix} =
    options {prefix = newPrefix}
  where
    newPrefix = T.concat [prefix, ".", toStrict . decodeUtf8 $ instanceId]
