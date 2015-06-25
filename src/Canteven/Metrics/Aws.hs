{-# LANGUAGE OverloadedStrings #-}

module Canteven.Metrics.Aws (
  instanceId
) where

import Control.Exception (catch)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client (Manager, httpLbs, parseUrl, Response(responseBody),
  HttpException)


-- | Return EC2 instance's id.
--
-- If the host is not an EC2 instance it returns an empty string.
instanceId :: Manager -> IO ByteString
instanceId manager =
    request manager url `catch` err
  where
    url = "http://instance-data/latest/meta-data/instance-id"
    err :: HttpException -> IO ByteString
    err = const $ return ""


-- | Send a HTTP request to @url@.
request :: Manager -> String -> IO ByteString
request manager url = do
  request <- parseUrl url
  response <- httpLbs request manager
  return $ responseBody response
