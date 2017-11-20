{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Hayoo.Server.Configuration
  ( -- * Server configuration
    Config (..)
  , ServerConfig (..)
  , HuntConfig (..)
  , defaultConfig
  , readTomlFile
  ) where


import           Data.Aeson
import qualified Data.Aeson.Types       as Json
import           Data.Monoid            (mconcat, (<>))
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           GHC.Generics
import           Hayoo.Internal.Helpers ((|>))
import qualified Hunt.Client            as HC
import           Servant.Client         (BaseUrl, parseBaseUrl)
import qualified Text.Parsec            as P
import qualified Text.Toml              as Toml



-- CONFIG


data Config
  = Config
    { server :: !ServerConfig
    , hunt   :: !HuntConfig
    } deriving (Show, Generic)


data ServerConfig
  = ServerConfig
    { host      :: !T.Text
    , port      :: !Int
    , publicDir :: !FilePath
    } deriving (Show, Generic)


data HuntConfig
  = HuntConfig
    { baseUrl :: !BaseUrl
    } deriving (Show, Generic)



-- PRIVATE HELPERS


formatParseError :: P.ParseError -> String
formatParseError parseError =
  let
    pos =
      P.errorPos parseError
  in
    mconcat
    [ "I could not read the provided configuration file. "
    , "Are you sure, this is a valid .toml configuration for Hayoo? "
    , "I found something troubling in \n\n"
    , "    line " ++ show (P.sourceLine pos) ++ "; column " ++ show (P.sourceColumn pos)
    , "\n\nYou can take a look at a valid example configuration here:\n"
    , "https://github.com/hunt-framework/hayoo/tree/master/hayoo-server/example.toml"
    ]



-- PUBLIC HELPERS


defaultConfig :: Config
defaultConfig = Config
  { server = ServerConfig "localhost" 3001 "public"
  , hunt = HuntConfig HC.huntBaseUrl
  }


readTomlFile :: FilePath -> IO (Either String Config)
readTomlFile file = do
  content <- T.readFile file
  pure $
    content
      |> Toml.parseTomlDoc ""
      |> fmap encode
      |> either (Left . formatParseError) pure
      >>= eitherDecode



-- INSTANCES


instance FromJSON Config
instance FromJSON ServerConfig
instance FromJSON HuntConfig where
  parseJSON (Object v) = do
    input <- v .: "baseUrl"
    case parseBaseUrl (T.unpack input) of
      Nothing ->
        fail ("The url `" ++ T.unpack input ++ "` is not a valid BaseUrl")

      Just parsedBaseUrl ->
        pure (HuntConfig parsedBaseUrl)

  parseJSON e =
    Json.typeMismatch "HuntConfig" e

