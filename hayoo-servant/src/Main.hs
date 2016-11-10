{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hayoo.Server


main :: IO ()
main = do
  runHayooServer serverConfig
