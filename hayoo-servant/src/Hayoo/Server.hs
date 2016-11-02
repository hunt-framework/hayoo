{-# LANGUAGE OverloadedStrings          #-}
module Hayoo.Server
  (
  ) where

import qualified Data.Text          as T
import           Hayoo.App


-- SERVER

data HayooConfig = HayooConfig
  { hayooHost   :: !T.Text
  , hayooPort   :: !Int
  , huntBaseUrl :: !SC.BaseUrl
  } deriving (Show)

