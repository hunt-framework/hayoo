{-# LANGUAGE OverloadedStrings #-}

module Hayoo.Server where


import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson.Types ()
import           Data.String (fromString)
import           Data.String.Conversions (cs)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Renderer.Utf8 as Blaze
import           Hunt.Server.Client (newServerAndManager)
import           Hunt.ClientInterface (LimitedResult)
import           Network.HTTP.Types.Status (internalServerError500, Status, notFound404, ok200)
import qualified Network.Wai.Middleware.RequestLogger as Wai
import qualified Network.Wai.Handler.Warp as W
import qualified System.Log.Logger as Log
import qualified System.Log.Formatter as Log (simpleLogFormatter)
import qualified System.Log.Handler as Log (setFormatter)
import qualified System.Log.Handler.Simple as Log (streamHandler)
import qualified System.IO as System (stdout)
import           Text.Read (readMaybe)
import qualified Web.Scotty.Trans as Scotty
import           Hayoo.Common
import qualified Hayoo.Templates as Templates
import           Paths_hayooFrontend
import qualified System.Metrics as EKG
import qualified System.Metrics.Counter as EKGC
import qualified System.Metrics.Distribution as EKGD
import qualified System.Metrics.Json as EKGJ

start :: HayooConfiguration -> IO ()
start config = do

    store               <- EKG.newStore
    EKG.registerGcMetrics store

    searchesCounter     <- EKG.createCounter "searches" store
    autocompleteCounter <- EKG.createCounter "autocompletes" store
    searchTimeDistr     <- EKG.createDistribution "search time" store
    let stats = Stats { statIncrSearchCounter       = EKGC.inc searchesCounter
                      , statIncrAutocompleteCounter = EKGC.inc autocompleteCounter
                      , statSearchTime              = EKGD.add searchTimeDistr
                      }

    sm <- newServerAndManager $ T.pack $ huntUrl config

    -- Note that 'runM' is only called once, at startup.
    let runM m = runHayooReader m stats sm

    initLoggers $ optLogLevel defaultOptions

    Log.debugM modName "Application start"

    let options =
          Scotty.Options
          { Scotty.verbose = 1
          , Scotty.settings =
              W.setPort (hayooPort config) $
              W.setHost (fromString $ hayooHost config) $
              W.defaultSettings
          }

    Scotty.scottyOptsT options runM $ do
        Scotty.middleware Wai.logStdoutDev -- request / response logging
        dispatcher store

fromFileWithMime :: FilePath -> TL.Text -> Scotty.ActionT HayooException HayooServer ()
fromFileWithMime path mime = do
    Scotty.setHeader "Content-Type" mime
    fullPath <- liftIO $ getDataFileName path
    Scotty.file fullPath


dispatcher :: EKG.Store -> Scotty.ScottyT HayooException HayooServer ()
dispatcher statStore = do
    Scotty.get "/"               $ (controlSimpleHtmlResults)
    Scotty.get "/json"           $ (controlSimpleResults Scotty.json)
    Scotty.get "/autocomplete"   $ handleAutocomplete `Scotty.rescue` (\_ -> Scotty.json ([]::[()]))
    Scotty.get "/opensearch.xml" $ fromFileWithMime "opensearch.xml" "application/opensearchdescription+xml"
    Scotty.get "/opensearch"     $ handleOpenSearch `Scotty.rescue` (\_ -> Scotty.json ([]::[()]))
    Scotty.get "/ajax/:page/"    $ controlAjaxResults
    Scotty.get "/packages/:package/badge" $ handleGetBadge
    Scotty.get "/hayoo.js"       $ fromFileWithMime "hayoo.js"    "text/javascript"
    Scotty.get "/hayoo.css"      $ fromFileWithMime "hayoo.css"   "text/css"
    Scotty.get "/hayoo.png"      $ fromFileWithMime "hayoo.png"   "image/png"
    Scotty.get "/hayoo2.png"     $ fromFileWithMime "hayoo2.png"   "image/png"
    Scotty.get "/favicon.ico"    $ fromFileWithMime "favicon.ico" "image/x-icon"
    Scotty.get "/examples"       $ Scotty.html $ Templates.body "" Templates.examples
    Scotty.get "/about"          $ Scotty.html $ Templates.body "" Templates.about
    Scotty.get "/stats"          $ handleStats statStore
    Scotty.notFound $ handleException "" FileNotFound

handleAutocomplete :: HayooAction ()
handleAutocomplete = do
    q <- Scotty.param "term"
    value <- autocomplete q
    Scotty.setHeader "Access-Control-Allow-Origin" "*"
    Scotty.json value

handleGetBadge :: HayooAction ()
handleGetBadge
  = do pkg      <- Scotty.param "package"
       mversion <- selectPackageVersion pkg
       case mversion of
        Nothing      -> Scotty.raise FileNotFound
        Just version' -> do
          Scotty.setHeader "Content-Type" "image/svg+xml"
          Scotty.raw (Blaze.renderMarkup (Templates.renderBadge version'))

handleOpenSearch :: HayooAction ()
handleOpenSearch = do
    q <- Scotty.param "term"
    value <- autocomplete q
    Scotty.setHeader "Access-Control-Allow-Origin" "*"
    Scotty.json (q, value)

handleStats :: EKG.Store -> HayooAction ()
handleStats store = do
  sample <- liftIO $ EKG.sampleAll store
  Scotty.json $ EKGJ.sampleToJson sample

getPage :: HayooAction Int
getPage = do
    params <- Scotty.params
    return $ maybe 0 id $ do
        page <- lookup "page" params
        page' <- readMaybe $ cs page
        return page'

controlAjaxResults :: HayooAction ()
controlAjaxResults = do
    page <- getPage
    q <- Scotty.param "query"
    results <- query (q) page
    Scotty.html $ Templates.ajax $ Templates.renderBoxedResults results

controlSimpleHtmlResults :: HayooAction ()
controlSimpleHtmlResults = controlResults render def handleException
    where
        render :: TL.Text -> LimitedResult SearchResult -> HayooAction ()
        render q r = Scotty.html $ Templates.body q (Templates.resultContent r)
        def = (Scotty.html $ Templates.body "" Templates.mainPage)

controlSimpleResults ::  (LimitedResult SearchResult -> HayooAction ()) -> HayooAction ()
controlSimpleResults repr = controlResults (\_ -> repr) (Scotty.raise "invalid Arguemtent")  (\_ _ -> Scotty.json ([]::[()]))

controlResults :: (TL.Text -> LimitedResult SearchResult -> HayooAction ()) -> HayooAction () -> (TL.Text -> HayooException -> HayooAction ()) -> HayooAction ()
controlResults repr emptyRepr exceptionHandler = do
    params <- Scotty.params
    page <- getPage
    case lookup "query" params of
        (Just q) -> do
            let
            ((query (cs q) page) >>= repr (cs q)) `Scotty.rescue`  exceptionHandler (cs q)
        Nothing -> emptyRepr

hayooExceptionToStatus :: HayooException -> Status
hayooExceptionToStatus ParseError{} = ok200
hayooExceptionToStatus FileNotFound{} = notFound404
hayooExceptionToStatus _ = internalServerError500

handleException :: TL.Text -> HayooException -> HayooAction ()
handleException q e = do
    Scotty.status $ hayooExceptionToStatus e
    Scotty.html $ Templates.body q $ Templates.renderException e

-- | Set the body of the response to the given 'T.Text' value. Also sets \"Content-Type\"
-- header to \"text/html\".
javascript :: (Scotty.ScottyError e, Monad m) => T.Text -> Scotty.ActionT e m ()
javascript t = do
    Scotty.setHeader "Content-Type" "text/javascript"
    Scotty.raw $ cs t

-- | Initializes the loggers with the given priority.
initLoggers :: Log.Priority -> IO ()
initLoggers level = do
    handlerBare <- Log.streamHandler System.stdout Log.DEBUG
    let handler = Log.setFormatter handlerBare $ Log.simpleLogFormatter "[$time : $loggername : $prio] $msg"

    Log.updateGlobalLogger "" (Log.setLevel level . Log.setHandlers [handler])
    rl <- Log.getRootLogger
    Log.saveGlobalLogger rl

data Options = Options
  { optLogLevel ::Log.Priority
  }

defaultOptions :: Options
defaultOptions = Options
  { optLogLevel = Log.DEBUG
  }

modName :: String
modName = "HayooFrontend"
