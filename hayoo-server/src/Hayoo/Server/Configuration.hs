{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Hayoo.Server.Configuration
  ( -- * Server configuration
    Config (..)
  , ServerConfig (..)
  , HuntConfig (..)
  , defaultConfig
  ) where


import qualified Data.Text      as T
import           GHC.Generics
import qualified Hunt.Client    as HC
import           Servant.Client (BaseUrl)



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



-- PUBLIC HELPERS


defaultConfig :: Config
defaultConfig = Config
  { server = ServerConfig "localhost" 3001 "public"
  , hunt = HuntConfig HC.huntBaseUrl
  }
