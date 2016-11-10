{-# LANGUAGE OverloadedStrings #-}
module Hayoo.Server
  ( HayooConfig (..)
  ) where

import qualified Data.Text as T
import           Hayoo.App
import           Servant



-- SERVER

data HayooConfig = HayooConfig
  { hayooHost      :: !T.Text
  , hayooPort      :: !Int
  , hayooPublicDir :: !FilePath
  , huntBaseUrl    :: !BaseUrl
  } deriving (Show)


runWithConfig :: HayooConfig -> IO ()
runWithConfig = undefined


server = undefined
