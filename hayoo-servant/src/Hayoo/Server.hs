{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Hayoo.Server
  ( -- * Types
    HayooConfig (..)

    -- * Server
  , runHayooServer
  , serverConfig
  ) where

import           Control.Monad.Except
import           Data.Maybe                (fromMaybe)
import qualified Data.Text                 as T
import           Hayoo.API
import           Hayoo.App
import           Hayoo.Types
import qualified Hunt.Client               as HC
import           Hunt.ClientInterface      (LimitedResult)
import           Network.Wai               (Application)
import           Network.Wai.Handler.Warp  (run)
import           Servant
import           Servant.Client            (BaseUrl)
import           Servant.Server
import           Servant.Utils.StaticFiles (serveDirectory)
import           System.Metrics            (Store)
import qualified System.Metrics            as EKG
import           System.Metrics.Json       (Sample (Sample))


-- SERVER

data HayooConfig = HayooConfig
  { hayooHost       :: !T.Text
  , hayooServerPort :: !Int
  , hayooPublicDir  :: !FilePath
  , huntBaseUrl     :: !BaseUrl
  } deriving (Show)


serverConfig :: HayooConfig
serverConfig = HayooConfig
  { hayooHost      = "localhost"
  , hayooServerPort      = 3001
  , hayooPublicDir = "public"
  , huntBaseUrl    = HC.huntBaseUrl
  }


-- API

runHayooServer :: HayooConfig -> IO ()
runHayooServer config = do
  store <- EKG.newStore
  EKG.registerGcMetrics store
  env <- HayooEnv <$> HC.withBaseUrl HC.huntBaseUrl <*> newMetrics store
  let port = hayooServerPort config
      server' = server store (hayooPublicDir config) env
  putStrLn $ "Starting hayoo server on port " ++ show port
  run port $ serve hayooAPI server'


server :: Store -> FilePath -> HayooEnv -> Server HayooAPI
server store path env = enter hayooAppToEither (serverT store)
                   :<|> serveDirectory path
  where
    hayooAppToEither :: HayooApp :~> ExceptT ServantErr IO
    hayooAppToEither = Nat $ \app -> do
      result <- liftIO $ runHayoo app env
      either (throwError . hayooErrToServantErr) return result

    hayooErrToServantErr :: HayooErr -> ServantErr
    hayooErrToServantErr _ = err500 { errBody = "Internal server error" }


serverT :: Store -> ServerT RestAPI HayooApp
serverT store = (measuredSearch :<|> measuredSearch)
           :<|> measuredCompletions
           :<|> (metrics store :<|> metrics store)
  where
    measuredSearch :: T.Text -> Maybe Int -> HayooApp (LimitedResult SearchResult)
    measuredSearch query p = measure searches (search query page)
      where page = fromMaybe 0 p

    measuredCompletions :: T.Text -> HayooApp [T.Text]
    measuredCompletions = measure completions . autocomplete
