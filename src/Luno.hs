{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Luno
  ( lunoClient
  , ticker
  , tickers
  ) where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client

import qualified Data.Text    as T
import qualified Data.Text.IO as T

-- |API endpoints
type LunoAPI = "ticker"  :> QueryParam "pair" T.Text :> Get '[JSON] Ticker
          :<|> "tickers" :> Get '[JSON] [Ticker]

type Price  = Float
type Pair   = T.Text
type Amount = Float

data Status = ACTIVE | POSTONLY | DISABLED deriving (Eq, Show, Read)

-- Luno uses strings instead of numbers in most cases
readPrice :: T.Text -> Price
readPrice = read . T.unpack

readAmount :: T.Text -> Amount
readAmount = read . T.unpack

readStatus :: T.Text -> Status
readStatus = read . T.unpack

-- | Ticker for a single currency pair
data Ticker = Ticker
  { ask                 :: Price
  , bid                 :: Price
  , lastTrade           :: Price
  , pair                :: Pair
  , rolling24HourVolume :: Amount
  , status              :: Status
  , timestamp           :: Integer
  } deriving (Eq, Show)

instance FromJSON Ticker where
  parseJSON (Object o) =
    Ticker  <$> (readPrice <$> o .: "ask")
            <*> (readPrice <$> o .: "bid")
            <*> (readPrice <$> o .: "last_trade")
            <*> o .: "pair"
            <*> (readAmount <$> o .: "rolling_24_hour_volume")
            <*> (readStatus <$> o .: "status")
            <*> o .: "timestamp"

lunoAPI :: Proxy LunoAPI
lunoAPI = Proxy

ticker :: Maybe Pair -> ClientM Ticker
tickers :: ClientM [Ticker]

ticker :<|> tickers = client lunoAPI

lunoClient = do
  manager <- newManager tlsManagerSettings
  return $ mkClientEnv manager (BaseUrl Https "api.luno.com" 443 "api/1")
