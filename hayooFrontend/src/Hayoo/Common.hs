{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}






module Hayoo.Common 
(
  ResultType (..)
, SearchResult (..)
, HayooServerT (..)
, HayooServer
, HayooException
-- , hayooServer
, runHayooReader
, runHayooReader'
, runHayooReaderInIO
, autocomplete
, query
, raiseExeptions
, HayooConfiguration (..)
) where

import           Prelude hiding (catch)
import           GHC.Generics (Generic)

import           Control.Applicative (Applicative)
import           Control.Exception (Exception, throwIO)
import           Control.Exception.Lifted (catch, catches, Handler (..))
import           Control.Failure (Failure, failure)

import           Control.Monad (liftM)
import           Control.Monad.Base (MonadBase, liftBase, liftBaseDefault)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Trans.Control (MonadBaseControl, StM, liftBaseWith, restoreM, ComposeSt, defaultLiftBaseWith, defaultRestoreM, MonadTransControl, StT, liftWith, restoreT, defaultLiftWith, defaultRestoreT)
import           Control.Monad.Reader (ReaderT, MonadReader, ask, runReaderT,)

import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Data (Data)

import           Data.Map (Map, fromList)
import           Data.String.Conversions (cs, (<>))
import           Data.Text (Text, isInfixOf)
import qualified Data.Text.Lazy as TL (Text)
import           Data.Typeable (Typeable)
import           Data.Vector ((!))

import           Network.HTTP.Conduit (HttpException)

#if MIN_VERSION_aeson(0,7,0)
import           Data.Scientific (Scientific)
#else
import           Data.Attoparsec.Number (Number (D))
#endif

import qualified System.Log.Logger as Log (debugM)

import           Text.Parsec (ParseError)

import qualified Web.Scotty.Trans as Scotty

import qualified Hunt.Server.Client as H
import           Hunt.Query.Language.Grammar (Query (..), BinOp (..), TextSearchType (..))

import           Hayoo.ParseSignature

data ResultType = Class | Data | Function | Method | Module | Newtype | Package | Type | Unknown
    deriving (Eq, Show, Generic)


instance FromJSON ResultType where
     parseJSON = genericParseJSON H.lowercaseConstructorsOptions


data SearchResult =
    NonPackageResult {
#if MIN_VERSION_aeson(0,7,0)    
        resultRank :: Scientific,
#else
        resultRank :: Double,
#endif
        resultUri :: (Map Text Text), 
        resultPackage :: Text,
        resultModule :: Text,
        resultName :: Text,
        resultSignature :: Text,
        resultDescription :: Text,
        resultSource :: Text,
        resultType :: ResultType
    }
    | PackageResult {
#if MIN_VERSION_aeson(0,7,0)    
        resultRank :: Scientific,
#else
        resultRank :: Double,
#endif
        resultUri :: (Map Text Text),  
        resultName :: Text,
        resultDependencies :: Text,
        resultMaintainer :: Text,
        resultSynopsis :: Text,
        resultAuthor :: Text,
        resultCategory :: Text,
        resultType :: ResultType
    }  deriving (Show, Eq)

parseUri :: (Monad m) => Text -> ByteString -> m (Map Text Text)
parseUri key t = 
    case eitherDecode t of
        Right v -> return v
        Left _ -> return $ fromList [(key, cs t)]

parsePackageResult rank descr baseUri n = do
    dep <- descr .:? "dependencies" .!= ""
    m  <- descr .:? "maintainer" .!= ""
    s  <- descr .:? "synopsis" .!= ""
    a  <- descr .:? "author" .!= ""
    cat  <- descr .:? "category" .!= ""
    u <- parseUri m baseUri -- unparsedUri
    return $ PackageResult rank u n dep m s a cat Package

parseNonPackageResult rank descr baseUri n d t = do
    p  <- descr .:? "package" .!= "unknown"
    m  <- descr .:? "module" .!= "Unknown"
    s  <- descr .:? "signature" .!= ""
    c  <- descr .:? "source" .!= ""
    u <- parseUri p baseUri -- unparsedUri
    return $ NonPackageResult rank  u p m n s d c t 

-- Partial Type Signature
-- parseSearchResult :: Double -> Value -> _
parseSearchResult rank (Object v) = do
    (Object descr) <- v .: "desc" -- This is always succesful. (as of january 2014)
    baseUri <- v .: "uri"
    -- unparsedUri <- descr
    
    n <- descr .:? "name" .!= ""
    d <- descr .:? "description" .!= ""
    t <- descr .:? "type" .!= Unknown
    case t of
        Package -> parsePackageResult rank descr baseUri n
        _       -> parseNonPackageResult rank descr baseUri n d t
parseSearchResult _ _ = fail "parseSearchResult: expected Object"

instance FromJSON SearchResult where
    parseJSON (Array v) = do
#if MIN_VERSION_aeson(0,7,0)    
        let (Number rank) = v ! 1
#else
        let (Number (D rank)) = v ! 1
#endif
            o = v ! 0
        parseSearchResult rank o
    parseJSON _ = fail "FromJSON SearchResult: Expected Tuple (Array) for SearchResult"


data HayooException = 
      ParseError ParseError
    | HuntClientException H.HuntClientException
    | HttpException HttpException
    | StringException Text  
    deriving (Show, Typeable)

instance Exception HayooException

instance Scotty.ScottyError HayooException where
    stringError s = StringException $ cs s

    showError e = cs $ show e

newtype HayooServerT m a = HayooServerT { runHayooServerT :: ReaderT H.ServerAndManager m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (H.ServerAndManager), MonadTrans)

type HayooServer = HayooServerT IO

-- from http://hackage.haskell.org/package/transformers-base-0.4.1/docs/src/Control-Monad-Base.html
instance (MonadBase b m) => MonadBase b (HayooServerT m) where
    liftBase = liftBaseDefault

-- from http://hackage.haskell.org/package/monad-control-0.3.2.3/docs/src/Control-Monad-Trans-Control.html
instance MonadTransControl (HayooServerT) where
    newtype StT (HayooServerT) a = StHayooServerT {unStHayooServerT :: StT (ReaderT H.ServerAndManager) a }
    liftWith = defaultLiftWith HayooServerT runHayooServerT StHayooServerT
    restoreT = defaultRestoreT HayooServerT unStHayooServerT

-- From http://hackage.haskell.org/package/monad-control-0.3.2.3/docs/src/Control-Monad-Trans-Control.html
instance (MonadBaseControl b m) => MonadBaseControl b (HayooServerT m) where 
    newtype StM (HayooServerT m) a = StMHayooServer {unStMHayooServer :: ComposeSt (HayooServerT) m a}
    liftBaseWith = defaultLiftBaseWith StMHayooServer
    restoreM     = defaultRestoreM   unStMHayooServer

runHayooReader :: HayooServerT m a -> H.ServerAndManager -> m a
runHayooReader = runReaderT . runHayooServerT

runHayooReaderInIO :: HayooServer a -> H.ServerAndManager -> IO a
runHayooReaderInIO x sm = runHayooReader x sm

runHayooReader' :: (MonadIO m) => HayooServerT m a -> Text -> m a
runHayooReader' x s = do
    sm <- H.newServerAndManager s
    runHayooReader x sm

-- , MonadBaseControl IO m
withServerAndManager' :: (MonadIO m, MonadBaseControl IO m) => H.HuntConnectionT (HayooServerT m) b -> HayooServerT m b
withServerAndManager' x = do
    sm <- ask
    --sm <- liftIO $ STM.readTVarIO var
    H.withServerAndManager x sm


-- ------------------------

handleSignatureQuery :: Text -> HayooServer (Either Text Query)
handleSignatureQuery q
    | "->" `isInfixOf` q = do
        s <- sig
        liftIO $ Log.debugM modName ("Signature Query: " <> (cs q) <> " >>>>>> " <> (show s))
        return $ Right s
    | otherwise = return $ Left q
        where
        -- sig = either (fail . show) (return . (<> "\"") . ("signature:\"" <>) . cs . prettySignature . fst . normalizeSignature) $ parseSignature $ cs q
        sig = case parseSignature $ cs q of
            (Right s) -> return sigQ
                where
                    q1 = QContext ["signature"] $ QWord QCase (cs $ prettySignature s)
                    q2 = QContext ["normalized"] $ QWord QCase (cs $ prettySignature $ fst $ normalizeSignature s)
                    sigQ = QBinary Or q1 q2
            (Left err) -> failure $ ParseError err

autocomplete :: Text -> HayooServer [Text]
autocomplete q = do
    q' <- handleSignatureQuery q
    withServerAndManager' $ either H.autocomplete (H.evalAutocomplete q) $ q'

-- , Failure HayooException m
query :: Text -> HayooServer (H.LimitedResult SearchResult)
query q = do
    q' <- handleSignatureQuery q
    withServerAndManager' $ either H.query H.evalQuery $ q'

raiseExeptions :: HayooServer a -> Scotty.ActionT HayooException HayooServer a
raiseExeptions x = do
    value <- (lift $ x) `catches` handlers
    return value
        where
        handlers
            = [
                Handler (\ (ex :: HayooException)   -> Scotty.raise ex),
                Handler (\ (ex :: H.HuntClientException)  -> Scotty.raise $ HuntClientException ex),
                Handler (\ (ex :: HttpException)    -> Scotty.raise $ HttpException ex)
            ]

data HayooConfiguration = HayooConfiguration {
    hayooHost :: String, 
    hayooPort :: Int, 
    huntUrl :: String
} deriving (Show, Data, Typeable)

modName :: String
modName = "HayooFrontend"