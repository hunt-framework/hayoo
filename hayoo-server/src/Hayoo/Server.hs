{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Hayoo.Server
  ( -- * Server
    runServer

    -- * Server Configuration
  , Hayoo.Config (..)
  , Hayoo.defaultConfig
  , Hayoo.readTomlFile
  ) where


import           Control.Monad.Except
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import           Hayoo.API
import qualified Hayoo.App                  as Hayoo
import qualified Hayoo.App.Page             as Page
import           Hayoo.App.Types
import           Hayoo.Internal.Helpers     ((|>))
import qualified Hayoo.Server.Configuration as Hayoo
import qualified Hunt.Client                as HC
import           Hunt.ClientInterface       (LimitedResult)
import           Network.Wai                (Application)
import           Network.Wai.Handler.Warp   (run)
import           Servant
import           Servant.Client             (BaseUrl)
import           Servant.Server
import           Servant.Utils.StaticFiles  (serveDirectoryFileServer)
import           System.FilePath            ((</>))
import           System.Metrics             (Store)
import qualified System.Metrics             as EKG
import           System.Metrics.Json        (Sample (Sample))
import qualified Text.Blaze.Html5           as H



-- SERVER


runServer :: Hayoo.Config -> IO ()
runServer config = do
  store <- EKG.newStore
  EKG.registerGcMetrics store
  env <- Hayoo.Env <$> HC.withBaseUrl (Hayoo.baseUrl (Hayoo.hunt config)) <*> Hayoo.newMetrics store
  let port = Hayoo.port (Hayoo.server config)
      server' = server store (Hayoo.publicDir (Hayoo.server config)) env
  putStrLn $ "Starting hayoo server on port " ++ show port
  run port $ serve hayooAPI server'


server :: Store -> FilePath -> Hayoo.Env -> Server HayooAPI
server store path env = hoistServer restAPI hayooAppToHandler (serverT store)
                   :<|> hoistServer htmlAPI hayooAppToHandler htmlServerT
                   :<|> serveDirectoryFileServer path
  where
    hayooAppToHandler :: Hayoo.App a -> Handler a
    hayooAppToHandler app = do
      result <- liftIO (Hayoo.run app env)
      either (throwError . hayooErrToServantErr) pure result

    hayooErrToServantErr :: Hayoo.Error -> ServantErr
    hayooErrToServantErr _ = err500 { errBody = "Internal server error" }


serverT :: Store -> ServerT RestAPI Hayoo.App
serverT store = searchAPI
           :<|> completionAPI
           :<|> metricsAPI store



-- APIS


searchAPI :: ServerT SearchAPI Hayoo.App
searchAPI = measuredSearch'
       :<|> measuredSearch
  where
    measuredSearch' :: Maybe T.Text -> Maybe Int -> Hayoo.App (LimitedResult SearchResult)
    measuredSearch' q = measuredSearch query
      where query = fromMaybe "" q

    measuredSearch :: T.Text -> Maybe Int -> Hayoo.App (LimitedResult SearchResult)
    measuredSearch query page =
      page
        |> fromMaybe 0
        |> Hayoo.search query
        |> Hayoo.measure Hayoo._searches


completionAPI :: ServerT AutocompleteAPI Hayoo.App
completionAPI maybeQuery =
  case maybeQuery of
    Nothing ->
      pure []

    Just query ->
      query
        |> Hayoo.autocomplete
        |> Hayoo.measure Hayoo._completions


metricsAPI :: Store -> ServerT MetricsAPI Hayoo.App
metricsAPI store = Hayoo.metrics store
              :<|> Hayoo.metrics store


htmlServerT :: ServerT HtmlAPI Hayoo.App
htmlServerT = ajax
         :<|> about
         :<|> examples
         :<|> index
  where
    ajax :: Int -> Maybe T.Text -> Hayoo.App H.Html
    ajax page maybeQuery =
      case maybeQuery of
        Nothing ->
          pure $
            Page.index Nothing Nothing

        Just query -> do
          result <- Hayoo.observe (Hayoo.measure Hayoo._searches (Hayoo.search query page))
          pure $ Page.viewSearchResults result


    about :: Hayoo.App H.Html
    about =
      pure Page.about

    examples :: Hayoo.App H.Html
    examples =
      pure Page.examples

    index :: Maybe T.Text -> Hayoo.App H.Html
    index maybeQuery =
      case maybeQuery of
        Nothing ->
          pure $
            Page.index Nothing Nothing

        Just query -> do
          result <- Hayoo.observe (Hayoo.measure Hayoo._searches (Hayoo.search query 0))
          pure $
            Page.index (Just (LT.fromStrict query)) (Just result)
