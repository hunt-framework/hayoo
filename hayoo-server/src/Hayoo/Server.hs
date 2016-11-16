{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Hayoo.Server
  ( -- * Server
    runHayooServer

    -- * Server Configuration
  ,  HayooServerConfiguration (..)
  , serverConfig
  , hayooConfig
  ) where

import           Control.Monad.Except
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import           Hayoo.API
import           Hayoo.App
import           Hayoo.Server.Configuration
import           Hayoo.Types
import qualified Hunt.Client                as HC
import           Hunt.ClientInterface       (LimitedResult)
import           Network.Wai                (Application)
import           Network.Wai.Handler.Warp   (run)
import           Servant
import           Servant.Client             (BaseUrl)
import           Servant.Server
import           Servant.Utils.StaticFiles  (serveDirectory)
import           System.Metrics             (Store)
import qualified System.Metrics             as EKG
import           System.Metrics.Json        (Sample (Sample))


-- SERVER

runHayooServer :: HayooServerConfiguration -> IO ()
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
serverT store = searchAPI
           :<|> completionAPI
           :<|> metricsAPI store


-- APIS

searchAPI :: ServerT SearchAPI HayooApp
searchAPI = measuredSearch'
       :<|> measuredSearch
  where
    measuredSearch' :: Maybe T.Text -> Maybe Int -> HayooApp (LimitedResult SearchResult)
    measuredSearch' q = measuredSearch query
      where query = fromMaybe "" q

    measuredSearch :: T.Text -> Maybe Int -> HayooApp (LimitedResult SearchResult)
    measuredSearch query p = measure searches (search query page)
      where page = fromMaybe 0 p


completionAPI :: ServerT AutocompleteAPI HayooApp
completionAPI =
  measure completions . autocomplete


metricsAPI :: Store -> ServerT MetricsAPI HayooApp
metricsAPI store = metrics store
              :<|> metrics store
