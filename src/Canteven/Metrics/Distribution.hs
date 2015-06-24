module Canteven.Metrics.Distribution (
  add
) where

import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)

import qualified System.Metrics.Distribution as D (Distribution, add)


-- | Add a sample @distribution@ started at @start@.
add :: D.Distribution -> UTCTime -> IO ()
add distribution start = do
  end <- getCurrentTime
  D.add distribution $ timeDiff end start


-- | Compute time difference to store in a metric distribution.
timeDiff :: Fractional a => UTCTime -> UTCTime -> a
timeDiff end = toDouble . diffUTCTime end
  where
    toDouble = fromRational . toRational
