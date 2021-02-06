{-# LANGUAGE OverloadedStrings #-}

module Luno.Common
  ( lunoClient
  , lunoQuery
  ) where

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client

lunoApiUrl = BaseUrl Https "api.luno.com" 443 "api/1"

{-|
  - Create a Luno client with no authentication
  - Can only be used for Market queries
-}
lunoClient :: IO ClientEnv
lunoClient = do
  manager <- newManager tlsManagerSettings
  return $ mkClientEnv manager lunoApiUrl

{-|
  - Perform a Luno query given a client
  - Used by other functions to perform actual API queries
-}
lunoQuery :: ClientEnv -> ClientM a -> IO (Either ClientError a)
lunoQuery env m = runClientM m env

