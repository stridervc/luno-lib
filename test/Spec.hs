{-# LANGUAGE OverloadedStrings #-}

import Luno
import Servant.Client
import qualified Data.Text as T

main :: IO ()
main = do
  client <- lunoClient
  res <- runClientM (ticker (Just "XBTZAR")) client

  putStrLn ""
  print res
