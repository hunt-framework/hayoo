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
import Hunt.Server.Client (newServerAndManager)

import Paths_hayooFrontend

type HayooAction = Scotty.ActionT HayooException HayooServer

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
    Scotty.get "/" $ (Scotty.params >>= renderRoot)
    Scotty.get "/hayoo.js" $ do
        Scotty.setHeader "Content-Type" "text/javascript"
        jsPath <- liftIO $ getDataFileName "hayoo.js"
        Scotty.file jsPath
    Scotty.get "/hayoo.css" $ do
        Scotty.setHeader "Content-Type" "text/css"
        cssPath <- liftIO $ getDataFileName "hayoo.css"
        Scotty.file cssPath
    Scotty.get "/autocomplete" $ handleAutocomplete `Scotty.rescue` (\_ -> Scotty.json ([]::[()]))
    Scotty.get "/examples" $ Scotty.html $ Templates.body "" Templates.examples
    Scotty.get "/about" $ Scotty.html $ Templates.body "" Templates.about

handleAutocomplete :: HayooAction ()
handleAutocomplete = do 
    q <- Scotty.param "term"
    value <- (raiseExeptions $ autocomplete $ q) -- >>= raiseOnLeft
    Scotty.json value

getPage :: [Scotty.Param] -> Int
getPage params = maybe 0 id $ do
    page <- lookup "page" params
    page' <- readMaybe $ cs page
    return page'

renderRoot :: [Scotty.Param] -> HayooAction ()
renderRoot params = renderRoot' $ TL.toStrict <$> lookup "query" params
    where 
    page = getPage params
    renderRoot' :: Maybe T.Text -> HayooAction ()
    renderRoot' Nothing = Scotty.html $ Templates.body "" Templates.mainPage
    renderRoot' (Just q) = renderRoot'' q `Scotty.rescue` (handleException q)

    renderRoot'' q = do
        results <- raiseExeptions $ query q page
        --Scotty.html $ Templates.body (cs q) $ Templates.renderLimitedRestults (cs q) value
        let
            mergedResults = mergeResults `convertResults` results
        Scotty.html $ Templates.body (cs q) $ Templates.renderMergedLimitedResults (cs q) mergedResults

handleException :: T.Text -> HayooException -> HayooAction ()
handleException q e = do
    Scotty.status internalServerError500
    Scotty.html $ Templates.body (cs q) $ Templates.renderException e
            
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


