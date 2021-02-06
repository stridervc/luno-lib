{-# LANGUAGE OverloadedStrings #-}

import Luno
import qualified Data.Text as T

main :: IO ()
main = do
  client <- lunoClient
  res <- ticker client "XBTZAR"

  putStrLn ""
  print res
