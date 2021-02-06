{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Luno
Description : Haskell module for luno.com API
Copyright   : (c) Fred Strauss, 2021

This module provides Haskell functions to access the luno.com API
-}

module Luno
  ( lunoClient
  , Price
  , Pair
  , Amount
  , Status
  , Ticker (..)
  , ticker
  , tickers
  ) where

import Luno.Common
import Luno.Market
