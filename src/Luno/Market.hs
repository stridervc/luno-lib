{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Luno.Market
  ( Price
  , Pair
  , Amount
  , Status
  , Ticker (..)
  , ticker
  , tickers
  ) where

import Luno.Common

import Data.Aeson
import Data.Proxy
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Generics
import Servant.API
import Servant.Client

import qualified Data.Text    as T
import qualified Data.Text.IO as T

type Price  = Float
type Pair   = T.Text
type Amount = Float
data Status = ACTIVE | POSTONLY | DISABLED deriving (Eq, Show, Read)

-- Luno uses strings instead of numbers in some cases
readFloat :: T.Text -> Float
readFloat = read . T.unpack

readStatus :: T.Text -> Status
readStatus = read . T.unpack

readTime :: Integer -> UTCTime
readTime t = posixSecondsToUTCTime $ fromInteger t / 1000

-- |Ticker for a single currency pair
data Ticker = Ticker
  { ask                 :: Price
  , bid                 :: Price
  , lastTrade           :: Price
  , pair                :: Pair
  , rolling24HourVolume :: Amount
  , status              :: Status
  , timestamp           :: UTCTime
  } deriving (Eq, Show)

instance FromJSON Ticker where
  parseJSON (Object o) =
    Ticker  <$> (readFloat <$> o .: "ask")
            <*> (readFloat <$> o .: "bid")
            <*> (readFloat <$> o .: "last_trade")
            <*> o .: "pair"
            <*> (readFloat <$> o .: "rolling_24_hour_volume")
            <*> (readStatus <$> o .: "status")
            <*> (readTime <$> o .: "timestamp")

-- |Market API endpoints
type LunoMarketAPI = "ticker"  :> QueryParam "pair" T.Text :> Get '[JSON] Ticker
          :<|> "tickers" :> Get '[JSON] [Ticker]

lunoMarketAPI :: Proxy LunoMarketAPI
lunoMarketAPI = Proxy

-- |API endpoint function type definitions
ticker'   :: Maybe Pair -> ClientM Ticker
tickers'  :: ClientM [Ticker]

-- |Create API endpoint functions
ticker' :<|> tickers' = client lunoMarketAPI

-- |Query ticker for given pair using client
ticker :: ClientEnv -> Pair -> IO (Either ClientError Ticker)
ticker client pair = lunoQuery client $ ticker' $ Just pair

-- |Query all tickers
tickers :: ClientEnv -> IO (Either ClientError [Ticker])
tickers client = lunoQuery client tickers'
