{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Monoid         ((<>))
import           Hayoo.Server
import           Options.Applicative

main :: IO ()
main = execParser opts >>= runHayooServer
  where
    opts = info (helper <*> hayooConfig)
      ( fullDesc
      <> progDesc "Start the Hayoo! server"
      <> header "A command line interface to configure the startup of the Hayoo! server" )
