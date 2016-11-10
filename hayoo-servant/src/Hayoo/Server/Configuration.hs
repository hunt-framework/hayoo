{-# LANGUAGE OverloadedStrings #-}
module Hayoo.Server.Configuration where

import qualified Data.Text      as T
import           Servant.Client (BaseUrl)
import qualified Hunt.Client as HC


-- TYPES

data HayooConfig = HayooConfig
  { hayooHost       :: !T.Text
  , hayooServerPort :: !Int
  , hayooPublicDir  :: !FilePath
  , huntBaseUrl     :: !BaseUrl
  } deriving (Show)


serverConfig :: HayooConfig
serverConfig = HayooConfig
  { hayooHost       = "localhost"
  , hayooServerPort = 3001
  , hayooPublicDir  = "public"
  , huntBaseUrl     = HC.huntBaseUrl
  }
