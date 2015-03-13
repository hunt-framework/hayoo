{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Hayoo.Index.Cabal
import Hayoo.Index.Hoogle
import Hayoo.Index.IndexSchema
import Hayoo.Index.PackageRank

import Conduit
import Control.Monad.State.Strict
import Control.Monad.Morph
import Data.Aeson (encode)
import Data.Text (pack)
import Hunt.Conduit
import Hunt.Server.Client
import Options.Applicative
import qualified Data.Map.Strict as Map

data Options = Options {
    hackageBase   :: String
  , hoogleArchive :: FilePath
  , indexArchive  :: FilePath
  , output        :: Output
  }

data Output = OutHunt String
            | OutJson

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
    ) <*> output
  where
    output =
      OutHunt `fmap` strOption (
        long "hunt-url"
        <> metavar "URI"
        <> help "Uri to hunt-server"
      )
      <|>
      const OutJson `fmap` switch (
        long "json"
        <> help "Output Hunt commands as json to stdout"
      )

main :: IO ()
main = do
  options <- execParser opts

  let hackageUri = mkHackageUri (hackageBase options)
      haddockUri = mkHaddockUri (hackageBase options)

      mkRank r p = Map.findWithDefault 1.0 (pack p) r

  case output options of
    OutJson -> do
      let pipe = indexCabalArchive hackageUri (indexArchive options)
                =$= mapC encode
                =$= unlinesAsciiC
                $$ stdoutC
      rk <- rankingStd <$> execStateT pipe []
      indexHoogleArchive haddockUri (mkRank rk) (hoogleArchive options)
        =$= mapC encode
        =$= unlinesAsciiC
        $$ stdoutC
      return ()
    OutHunt uri -> do
      sam <- newServerAndManager (pack uri)
      withServerAndManager sam $ do
        _ :: String <- postCommand dropHayooIndexSchema
        _ :: String <- postCommand createHayooIndexSchema
        let pipe = indexCabalArchive hackageUri (indexArchive options)
                   $$ (hoist lift cmdSink)
        rk <- rankingStd <$> execStateT pipe []
        indexHoogleArchive  haddockUri (mkRank rk) (hoogleArchive options) $$ cmdSink
      return ()
  where
    opts = info (helper <*> indexerOptions)
           (fullDesc
            <> progDesc "Indexes hoogle.tar.gz and index.tar.gz from Hackage"
            <> header "hayoo-indexer - a hackage indexer")
