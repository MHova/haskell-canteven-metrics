{-# LANGUAGE OverloadedStrings #-}

module Canteven.Metrics.Types (
  MetricsConfig(MetricsConfig, ekgHost, ekgPort, carbon),
  CarbonConfig(CarbonConfig, carbonOptions)
) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson (FromJSON, parseJSON, Value(Object), (.:?), (.:), (.!=))
import Data.ByteString (ByteString)
import System.Remote.Monitoring.Carbon (CarbonOptions(CarbonOptions))


-- | Cantevent metrics configuration.
data MetricsConfig = MetricsConfig {
    ekgHost :: String,
    ekgPort :: Int,
    carbon :: Maybe CarbonConfig
  } deriving (Show)

instance FromJSON MetricsConfig where
  parseJSON (Object topLevel) = do
    mMetrics <- topLevel .:? "metrics"
    case mMetrics of
      Nothing -> return defaultMetricsConfig
      Just config -> MetricsConfig
        <$> config .: "host" .!= "0.0.0.0"
        <*> config .: "port" .!= 8081
        <*> config .:? "carbon"
  parseJSON value =
    fail $ "Couldn't parse metrics config from " ++ show value


-- | Default metrics configuration.
defaultMetricsConfig :: MetricsConfig
defaultMetricsConfig = MetricsConfig {
    ekgHost = "0.0.0.0",
    ekgPort = 8081,
    carbon = Nothing
  }


-- | Wrap @CarbonOptions@ to avoid orphan instances.
data CarbonConfig = CarbonConfig {
    carbonOptions :: CarbonOptions
  } deriving (Show)

instance FromJSON CarbonConfig where
  parseJSON (Object config) = CarbonConfig <$> options
    where
      options = CarbonOptions
        <$> config .: "host" .!= "127.0.0.1"
        <*> config .: "port" .!= 2003
        <*> config .: "flushInterval" .!= 1000
        <*> config .: "prefix" .!= ""
        <*> config .: "suffix" .!= ""
  parseJSON value =
    fail $ "Couldn't parse Carbon config from " ++ show value
