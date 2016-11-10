{-# LANGUAGE OverloadedStrings #-}
module Hayoo.Server.Configuration
  ( -- * Server configuration
    HayooServerConfiguration (..)
  , serverConfig

    -- * Command line parser
  , hayooConfig
  ) where

import qualified Data.Text           as T
import qualified Hunt.Client         as HC
import           Options.Applicative
import           Servant.Client      (BaseUrl, parseBaseUrl)


-- TYPES

data HayooServerConfiguration = HayooServerConfiguration
  { hayooServerHost :: !T.Text
  , hayooServerPort :: !Int
  , hayooPublicDir  :: !FilePath
  , huntBaseUrl     :: !BaseUrl
  } deriving (Show)


serverConfig :: HayooServerConfiguration
serverConfig = HayooServerConfiguration
  { hayooServerHost = "localhost"
  , hayooServerPort = 3001
  , hayooPublicDir  = "public"
  , huntBaseUrl     = HC.huntBaseUrl
  }


-- PARSER

hayooConfig :: Parser HayooServerConfiguration
hayooConfig =
  HayooServerConfiguration
  <$> hostOption
  <*> portOption
  <*> publicDirOption
  <*> huntUrlOption
  where
    defaultHost      = hayooServerHost serverConfig
    defaultPort      = hayooServerPort serverConfig
    defaultPublicDir = hayooPublicDir serverConfig
    defaultHuntUrl   = huntBaseUrl serverConfig

    hostOption = option auto
      ( long "host"
      <> short 'h'
      <> metavar "HOST"
      <> help ("Host of Hayoo! server, defaults to '" ++ (T.unpack defaultHost) ++ "'"))
      <|> pure defaultHost

    portOption = option auto
      ( long "port"
      <> short 'p'
      <> metavar "PORT"
      <> help ("Port to run the server on, defaults to " ++ show defaultPort))
      <|> pure defaultPort

    publicDirOption = option auto
      ( long "static-dir"
      <> short 'd'
      <> metavar "DIR"
      <> help ("Directory for static assets, defaults to \"$(pwd)/public\""))
      <|> pure defaultPublicDir

    huntUrlOption = option parseUrl
     ( long "hunt-server-url"
      <> short 's'
      <> metavar "URL"
      <> value defaultHuntUrl
      <> (help $ "Base URL of the backing Hunt server. Defaults to " ++ show defaultHuntUrl ))
      where
        parseUrl = eitherReader $
          either (Left . show) Right . parseBaseUrl

