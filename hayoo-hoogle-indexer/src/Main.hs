{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent.Async
import Data.Text (pack)
import Hayoo.Index.Cabal
import Hayoo.Index.Hoogle
import Hayoo.Index.IndexSchema
import Hunt.Server.Client
import Options.Applicative

data Options = Options {
    hackageBase :: String
  , hoogleArchive :: FilePath
  , indexArchive  :: FilePath
  , huntServer    :: String
  }

strOption' x a = strOption x <|> pure a

indexerOptions =
  Options <$> strOption (
    long "hackage-url"
    <> metavar "URL"
    <> help "Hackage server address"
    ) <*> strOption (
    long "hoogle"
    <> metavar "FILE"
    <> help "Haddocks Hoogle output"
    ) <*> strOption (
    long "index"
    <> metavar "FILE"
    <> help "Hackage index.tar.gz"
    ) <*> strOption' (
    long "hunt-url"
    <> metavar "URI"
    <> help "Uri to hunt-server"
    ) "http://localhost:3000"

main :: IO ()
main = do
  options <- execParser opts
  sam     <- newServerAndManager (pack (huntServer options))

  let processHoogleArchive =
        indexHoogleArchive (
          mkHaddockUri (hackageBase options)) (hoogleArchive options)

  let processCabalArchive =
        indexCabalArchive (
          mkHackageUri (hackageBase options)) (indexArchive options)

  withServerAndManager sam $ do
    _ :: String <- postCommand dropHayooIndexSchema
    _ :: String <- postCommand createHayooIndexSchema
    return ()

  _ <- concurrently
       (withServerAndManager sam processCabalArchive)
       (withServerAndManager sam processHoogleArchive)
  return ()
  where
    opts = info (helper <*> indexerOptions)
           (fullDesc
            <> progDesc "Indexes hoogle.tar.gz and index.tar.gz from Hackage"
            <> header "hayoo-indexer - a hackage indexer")
