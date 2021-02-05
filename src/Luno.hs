{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Luno
  (
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Servant.API
import Servant.Client

import qualified Data.Text    as T
import qualified Data.Text.IO as T

-- |API endpoints
type LunoAPI =
  "tickers" :> Get '[JSON] [Ticker]

type Price  = Text
type Pair   = Text
type Amount = Text
type Status = Text

-- |Ticker for a single pair
data Ticker = Ticker
  { ask                 :: Price
  , bid                 :: Price
  , lastTrade           :: Price
  , pair                :: Pair
  , rolling24HourVolume :: Amount
  , status              :: Status
  , timestamp           :: Text
  } deriving (Eq, Show)

instance FromJSON Ticker where
  parseJSON (Object o) =
    Ticker  <$> o .: "ask"
            <*> o .: "bid"
            <*> o .: "last_trade"
            <*> o .: "pair"
            <*> o .: "rolling_24_hour_volume"
            <*> o .: "status"
            <*> o .: "timestamp"

  parseJSON _ = mzero

lunoAPI :: Proxy LunoAPI
lunoAPI = Proxy

-- getTickers :: ExceptT ServantError IO [Ticker]
getTickers = client lunoAPI -- (BaseUrl Https "https://api.luno.com/api/1/" 443)
