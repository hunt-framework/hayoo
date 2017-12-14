module Hayoo.Pump
  ( Config (..)
  , pump
  ) where


import           Servant.Client (BaseUrl)



-- PUMPING


data Config
  = Config
    { indexDirectory :: FilePath
    , huntBaseUrl    :: BaseUrl
    } deriving (Show)


-- | @pump@ pumps data from previously indexed files
-- by @index@ into the hunt-server.
pump :: Config -> IO ()
pump =
  undefined
