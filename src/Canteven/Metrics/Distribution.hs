module Canteven.Metrics.Distribution (
  sample
) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)

import qualified System.Metrics.Distribution as D (Distribution, add)


-- | Sample execution of @action@ to @distribution@.
sample :: MonadIO m => D.Distribution -> m a -> m a
sample distribution action = do
  start <- liftIO getCurrentTime
  result <- action
  end <- liftIO getCurrentTime
  liftIO $ D.add distribution $ timeDiff end start
  return result


-- | Compute time difference to store in a metric distribution.
timeDiff :: Fractional a => UTCTime -> UTCTime -> a
timeDiff end = toDouble . diffUTCTime end
  where
    toDouble = fromRational . toRational
