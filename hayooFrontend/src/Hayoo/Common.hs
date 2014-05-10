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
, HayooAction 
-- , hayooServer
, runHayooReader
, runHayooReader'
, autocomplete
, query
, queryMore
, HayooConfiguration (..)
-- ----
, stablePartitionBy
, mergeResults
, ModuleResult
, PackageResult
, convertResults
-- --------
, DisplayType (..)
) where

import           GHC.Generics (Generic)

import           Control.Applicative (Applicative)
import           Control.Exception (Exception, throwIO)
import           Control.Exception.Lifted (catches, Handler (..))
--import           Control.Failure (Failure, failure)
import           Control.Monad.Base (MonadBase, liftBase, liftBaseDefault)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Trans.Control (MonadBaseControl, StM, liftBaseWith, restoreM, ComposeSt, defaultLiftBaseWith, defaultRestoreM, MonadTransControl, StT, liftWith, restoreT, defaultLiftWith, defaultRestoreT)
import           Control.Monad.Reader (ReaderT, MonadReader, ask, runReaderT,)

import           Data.Aeson
import           Data.Data (Data)
import           Data.List (partition)
import           Data.Scientific (Scientific)
import           Data.String (IsString, fromString)
import           Data.String.Conversions (cs, (<>))
import           Data.Text (Text, isInfixOf)
import           Data.Typeable (Typeable)
--import           Data.Vector ((!))

import qualified Hunt.Server.Client as H
import           Hunt.Query.Language.Grammar (Query (..), BinOp (..), TextSearchType (..))
import           Hunt.Query.Language.Parser (parseQuery)

import           Network.HTTP.Conduit (HttpException)

import qualified System.Log.Logger as Log (debugM)

import           Text.Parsec (ParseError)

import qualified Web.Scotty.Trans as Scotty

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
        resultScore :: Scientific,
#else
        resultScore :: Double,
#endif
        resultUri :: Text, 
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
        resultScore :: Scientific,
#else
        resultScore :: Double,
#endif
        resultUri :: Text,  
        resultName :: Text,
        resultDependencies :: Text,
        resultMaintainer :: Text,
        resultSynopsis :: Text,
        resultAuthor :: Text,
        resultCategory :: Text,
        resultType :: ResultType
    }  deriving (Show, Eq, Generic)


parsePackageResult score descr baseUri n = do
    dep <- descr .:? "dependencies" .!= ""
    m  <- descr .:? "maintainer" .!= ""
    s  <- descr .:? "synopsis" .!= ""
    a  <- descr .:? "author" .!= ""
    cat  <- descr .:? "category" .!= ""
--    u <- baseUri -- unparsedUri
    return $ PackageResult score baseUri n dep m s a cat Package

parseNonPackageResult score descr baseUri n d t = do
    p  <- descr .:? "package" .!= "unknown"
    m  <- descr .:? "module" .!= "Unknown"
    s  <- descr .:? "signature" .!= ""
    c  <- descr .:? "source" .!= ""
--    u <- baseUri -- unparsedUri
    return $ NonPackageResult score  baseUri p m n s d c t 

-- Partial Type Signature
-- parseSearchResult :: Double -> Value -> _
parseSearchResult (Object v) = do
    (Object descr) <- v .: "description" -- This is always succesful. (as of january 2014)
    (Number score) <- v .:? "score" .!= (Number 1.0)
    baseUri <- v .: "uri"
    -- unparsedUri <- descr
    
    n <- descr .:? "name" .!= ""
    d <- descr .:? "description" .!= ""
    t <- descr .:? "type" .!= Unknown
    case t of
        Package -> parsePackageResult score descr baseUri n
        _       -> parseNonPackageResult score descr baseUri n d t
parseSearchResult _ = fail "parseSearchResult: expected Object"

instance FromJSON SearchResult where
    parseJSON o = parseSearchResult o
--    parseJSON _ = fail "FromJSON SearchResult: Expected Tuple (Array) for SearchResult"

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

type HayooAction = Scotty.ActionT HayooException HayooServer

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

handleSignatureQuery :: (Monad m, MonadIO m) => Text -> m Query
handleSignatureQuery q
    | "->" `isInfixOf` q = do
        s <- sig
        liftIO $ Log.debugM modName ("Signature Query: " <> (cs q) <> " >>>>>> " <> (show s))
        return s
    | otherwise = normalQuery
        where
        sig = case parseSignature $ cs q of
            (Right s) -> return sigQ
                where
                    q1 = QContext ["signature"] $ QWord QCase (cs $ prettySignature s)
                    q2 = QContext ["normalized"] $ QWord QCase (cs $ prettySignature $ fst $ normalizeSignature s)
                    sigQ = QBinary Or q1 q2
            (Left err) -> liftIO $ throwIO $ ParseError err
        normalQuery = case parseQuery (cs q) of 
            (Right q') -> return q'
            (Left err) -> liftIO $ throwIO $ StringException err

autocomplete :: Text -> HayooAction [Text]
autocomplete q = raiseExeptions $ do
    q' <- handleSignatureQuery q
    withServerAndManager' $ H.evalAutocomplete q q'

query :: Text -> Int -> HayooAction (H.LimitedResult SearchResult)
query q p = raiseExeptions $ do
    q' <- handleSignatureQuery q
    withServerAndManager' $ H.evalQuery q' (p * 20)

queryMore :: Text -> Text -> Text -> Int -> HayooAction (H.LimitedResult SearchResult)
queryMore package m' q p = raiseExeptions $ do
    q' <- handleSignatureQuery q
    let qq = foldr1 (QBinary And) [q', qm, qp]
    withServerAndManager' $ H.evalQuery qq (p * 20) 
    where
    qp = QContext ["package"] $ QWord QCase package
    qm = QContext ["module"] $ QWord QCase m'    

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


-- | /O(n^2)/. An alternative implementaion would be something like groupBy $ stableSortBy 
stablePartitionBy :: (a -> a -> Bool) -> [a] -> [[a]]
stablePartitionBy _ [] = []
stablePartitionBy f (x:xs) = (x : xsIn) : stablePartitionBy f xsOut
    where 
    (xsIn, xsOut) = partition (f x) xs

mergeResults :: [SearchResult] -> [PackageResult]
mergeResults [] = []
mergeResults (x:xs) 
    | resultType x == Package = (Just x, mergeModules moduleResults) : mergeResults rest
    | not $ null packages     = mergeResults $ packages ++ (x:rest')
    | otherwise               = (Nothing,  mergeModules $ x:moduleResults') : mergeResults rest''
        where
        (moduleResults,rest)    = partition (\r -> (resultType r /= Package) && (resultName x)    == (resultPackage r)) xs
        (packages,rest')        = partition (\r -> (resultType r == Package) && (resultPackage x) == (resultName r)) xs
        (moduleResults',rest'') = partition (\r -> (resultType r /= Package) && (resultPackage x) == (resultPackage r)) xs


mergeModules :: [SearchResult] -> [ModuleResult]
mergeModules [] = []
mergeModules (x:xs) 
    | resultType x == Module  = (Just x, results) : mergeModules rest
    | not $ null modules      = mergeModules $ modules ++ (x:rest')
    | otherwise               = (Nothing,  x:results') : mergeModules rest''
        where
        (results,rest)          = partition (\r -> (resultType x /= Module) && (resultModule x) == (resultModule r)) xs
        (modules,rest')         = partition (\r -> (resultType x == Module) && (resultModule x) == (resultModule r)) xs
        (results',rest'')       = partition (\r -> (resultType x /= Module) && (resultModule x) == (resultModule r)) xs

convertResults :: ([a] -> [b]) -> H.LimitedResult a -> H.LimitedResult b
convertResults f (H.LimitedResult r x y z) = H.LimitedResult (f r) x y z 

-- -------------------------------

data DisplayType = Grouped | Boxed