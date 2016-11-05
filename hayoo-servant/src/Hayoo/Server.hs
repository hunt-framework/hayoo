{-# LANGUAGE OverloadedStrings #-}
module Hayoo.Server
  ( HayooConfig (..)
  ) where

import qualified Data.Text      as T
import           Hayoo.App
import           Servant.Client (BaseUrl)


-- SERVER

data HayooConfig = HayooConfig
  { hayooHost   :: !T.Text
  , hayooPort   :: !Int
  , huntBaseUrl :: !BaseUrl
  } deriving (Show)


