{-# LANGUAGE OverloadedStrings #-}

module Hayoo.Server where


import           Control.Monad.IO.Class (liftIO)
import           Control.Applicative ((<$>))

import           Data.Aeson.Types ()
import           Data.String (fromString)
import           Data.String.Conversions (cs)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           Network.HTTP.Types.Status (internalServerError500)
import qualified Network.Wai.Middleware.RequestLogger as Wai
import qualified Network.Wai.Handler.Warp as W

import qualified System.Log.Logger as Log
import qualified System.Log.Formatter as Log (simpleLogFormatter)
import qualified System.Log.Handler as Log (setFormatter)
import qualified System.Log.Handler.Simple as Log (streamHandler)
import qualified System.IO as System (stdout)

import           Text.Read (readMaybe)

import qualified Web.Scotty.Trans as Scotty

import qualified Hayoo.Templates as Templates

import Hayoo.Common
import Hunt.Server.Client (newServerAndManager, LimitedResult)

import Paths_hayooFrontend

start :: HayooConfiguration -> IO ()
start config = do
    sm <- newServerAndManager $ T.pack $ huntUrl config

    -- Note that 'runM' is only called once, at startup.
    let runM m = runHayooReader m sm
        runActionToIO = runM

    initLoggers $ optLogLevel defaultOptions

    Log.debugM modName "Application start"

    let options = Scotty.Options {Scotty.verbose = 1, Scotty.settings = (W.defaultSettings { W.settingsPort = hayooPort config, W.settingsHost = fromString $ hayooHost config })}

    Scotty.scottyOptsT options runM runActionToIO $ do
        Scotty.middleware Wai.logStdoutDev -- request / response logging
        dispatcher      

dispatcher :: Scotty.ScottyT HayooException HayooServer ()
dispatcher = do
    Scotty.get "/"       $ (controlGroupedResults)
    Scotty.get "/json"   $ (controlSimpleResults Scotty.json)
--    Scotty.get "/ajax"   $ (controlSimpleResults (Scotty.html . Templates.ajax . Templates.renderResults))
    Scotty.get "/simple" $ (controlSimpleHtmlResults)
    Scotty.get "/autocomplete" $ handleAutocomplete `Scotty.rescue` (\_ -> Scotty.json ([]::[()]))
    Scotty.get "/ajax/:package/:module/:query"   $ controlAjaxResults

    Scotty.get "/hayoo.js" $ do
        Scotty.setHeader "Content-Type" "text/javascript"
        jsPath <- liftIO $ getDataFileName "hayoo.js"
        Scotty.file jsPath
    Scotty.get "/hayoo.css" $ do
        Scotty.setHeader "Content-Type" "text/css"
        cssPath <- liftIO $ getDataFileName "hayoo.css"
        Scotty.file cssPath
    Scotty.get "/examples" $ Scotty.html $ Templates.body "" Boxed Templates.examples
    Scotty.get "/about" $ Scotty.html $ Templates.body "" Boxed Templates.about 
    Scotty.notFound $ handleException "" FileNotFound

handleAutocomplete :: HayooAction ()
handleAutocomplete = do 
    q <- Scotty.param "term"
    value <- autocomplete q
    Scotty.json value

getPage :: HayooAction Int
getPage = do
    params <- Scotty.params 
    return $ maybe 0 id $ do
        page <- lookup "page" params
        page' <- readMaybe $ cs page
        return page'

controlGroupedResults :: HayooAction ()
controlGroupedResults  = controlResults renderMerged (Scotty.html $ Templates.body "" Boxed Templates.mainPage) handleException 
    where
    renderMerged q results = Scotty.html $ Templates.body (cs q) Boxed $ Templates.renderMergedLimitedResults (cs q) mergedResults
        where
        mergedResults = mergeResults `convertResults` results

controlAjaxResults :: HayooAction ()
controlAjaxResults = do
    package <- Scotty.param "package"
    m' <- Scotty.param "module"
    query <- Scotty.param "query"
    page <- getPage
    results <- queryMore package m' query page
    Scotty.html $ Templates.ajax $ Templates.renderResults results

controlSimpleHtmlResults :: HayooAction ()
controlSimpleHtmlResults = controlResults render def handleException 
    where
        render :: TL.Text -> LimitedResult SearchResult -> HayooAction ()
        render q r = Scotty.html $ Templates.body q Grouped (Templates.renderBoxedResults r)
        def = (Scotty.html $ Templates.body "" Grouped Templates.mainPage)

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
    


handleException :: TL.Text -> HayooException -> HayooAction ()
handleException q e = do
    Scotty.status internalServerError500
    Scotty.html $ Templates.body q Boxed $ Templates.renderException e
            
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


