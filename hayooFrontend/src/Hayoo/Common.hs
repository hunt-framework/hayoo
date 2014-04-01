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
, HayooException (..)
-- , hayooServer
, runHayooReader
, runHayooReader'
, autocomplete
, query
, raiseExeptions
, HayooConfiguration (..)
-- ----
, stablePartitionBy
, mergeResults
, ModuleResult
, PackageResult
, convertResults
) where

import           GHC.Generics (Generic)

import           Control.Applicative (Applicative)
import           Control.Exception (Exception)
import           Control.Exception.Lifted (catches, Handler (..))
import           Control.Failure (Failure, failure)

import           Control.Applicative ((<$>))
import           Control.Monad.Base (MonadBase, liftBase, liftBaseDefault)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Trans.Control (MonadBaseControl, StM, liftBaseWith, restoreM, ComposeSt, defaultLiftBaseWith, defaultRestoreM, MonadTransControl, StT, liftWith, restoreT, defaultLiftWith, defaultRestoreT)
import           Control.Monad.Reader (ReaderT, MonadReader, ask, runReaderT,)

import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Data (Data)
import           Data.Function (on)

import           Data.List (groupBy, partition, find)
import           Data.Map (Map, fromList)
import           Data.String (IsString, fromString)
import           Data.String.Conversions (cs, (<>))
import           Data.Text (Text, isInfixOf)
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

instance ToJSON ResultType where
    toJSON = genericToJSON H.lowercaseConstructorsOptions

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
    }  deriving (Show, Eq, Generic)

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

instance ToJSON SearchResult where

data HayooException = 
      ParseError ParseError
    | HuntClientException H.HuntClientException
    | HttpException HttpException
    | StringException Text  
    | FileNotFound
    deriving (Show, Typeable)

instance Exception HayooException

instance Scotty.ScottyError HayooException where
    stringError s = StringException $ cs s

    showError e = cs $ show e

instance IsString HayooException where
        fromString s = StringException $ cs s

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

runHayooReader' :: (MonadIO m) => HayooServerT m a -> Text -> m a
runHayooReader' x s = do
    sm <- H.newServerAndManager s
    runHayooReader x sm

withServerAndManager' :: (MonadIO m, MonadBaseControl IO m) => H.HuntConnectionT (HayooServerT m) b -> HayooServerT m b
withServerAndManager' x = do
    sm <- ask
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

query :: Text -> Int -> HayooServer (H.LimitedResult SearchResult)
query q p = do
    q' <- handleSignatureQuery q
    withServerAndManager' $ ((either H.query H.evalQuery) q' (p * 20) )

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

-- --------------------------

type ModuleResult = (Maybe SearchResult, [SearchResult])
type PackageResult = (Maybe SearchResult, [ModuleResult])

extract :: ResultType -> [SearchResult] -> ([SearchResult], [SearchResult])
extract t = partition (\sr -> t == resultType sr)


-- | /O(n^2)/. An alternative implementaion would be something like groupBy $ stableSortBy 
stablePartitionBy :: (a -> a -> Bool) -> [a] -> [[a]]
stablePartitionBy _ [] = []
stablePartitionBy f (x:xs) = (x : xsIn) : stablePartitionBy f xsOut
    where 
    (xsIn, xsOut) = partition (f x) xs


mergeResults :: [SearchResult] -> [PackageResult]
mergeResults srs = packageResults'
    where
    (packages,rest') = extract Package srs
    (modules, rest)  = extract Module rest'
    moduleResults :: [[SearchResult]]
    moduleResults    = stablePartitionBy ((==) `on` resultModule) rest
    moduleResults' :: [ModuleResult]
    moduleResults'   = (\srs' -> (find (\ mr -> resultModule mr == (resultModule $ head srs')) modules, srs')) <$> moduleResults
    packageResults :: [[ModuleResult]]
    packageResults   = stablePartitionBy ((==) `on` (resultPackage . head . snd)) moduleResults'
    packageResults' :: [PackageResult]
    packageResults'  = (\srs' -> (find (\ mr -> resultPackage mr == (resultPackage $ head $ snd $ head srs')) modules, srs')) <$> packageResults

convertResults :: ([a] -> [b]) -> H.LimitedResult a -> H.LimitedResult b
convertResults f (H.LimitedResult r x y z) = H.LimitedResult (f r) x y z 
