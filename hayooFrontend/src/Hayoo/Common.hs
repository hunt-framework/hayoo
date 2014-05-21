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
, HayooConfiguration (..)
-- -------
, printQuery
, contextQueryToQuery
, contextQueryName
, ContextQuery ()
, contextQueries
) where

import           GHC.Generics (Generic)

import           Control.Applicative (Applicative, (<$>), (<*>), liftA2)
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
import           Data.Text (Text, isInfixOf, splitOn, strip)
import           Data.Typeable (Typeable)
--import           Data.Vector ((!))

import           Hunt.Common.BasicTypes (Context)
import qualified Hunt.Server.Client as H
import           Hunt.ClientInterface (qOrs, qAnd, qContext, qPhrase, qWord, withinContexts)
import           Hunt.Query.Language.Grammar (Query (..), BinOp (..), TextSearchType (..), printQuery)
import           Hunt.Query.Language.Parser (parseQuery)

import           Network.HTTP.Conduit (HttpException)

import qualified System.Log.Logger as Log (debugM)

import           Text.Parsec (ParseError)
import           Text.Read (readMaybe)

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
        resultModules :: [Text],
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

getSRPackage :: SearchResult -> Text
getSRPackage sr@NonPackageResult{} = resultPackage sr
getSRPackage sr@PackageResult{} = resultName sr

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
    m  <- descr .:? "module" .!= "[]"
    s  <- descr .:? "signature" .!= ""
    c  <- descr .:? "source" .!= ""
--    u <- baseUri -- unparsedUri
    let mods :: [Text]
        mods = maybe [m] id (readMaybe $ cs m)
    return $ NonPackageResult score  baseUri p mods n s d c t 

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

-- ------------------------------
data ContextQuery
    = QueryReverseDependencies
    | QueryPackageModules
    | QueryPackageDatatypes
    | QueryPackageByAuthor
    | QueryModuleContent
    | QueryPackage
    | QueryModule
    deriving (Show)

contextQueryToQuery :: ContextQuery -> SearchResult -> Query
contextQueryToQuery (QueryReverseDependencies) sr = mkContext "dependencies" $ getSRPackage sr
contextQueryToQuery (QueryPackageModules)      sr = QBinary And (mkContext "package" $ getSRPackage sr) (mkContext "type" "module")
contextQueryToQuery (QueryPackageDatatypes)    sr = QBinary And (mkContext "package" $ getSRPackage sr) (qOrs $ (mkContext "type") <$> ["data", "newtype", "type"])
contextQueryToQuery (QueryPackageByAuthor)     sr@PackageResult{} = qOrs $ (withinContexts ["author"] . qPhrase) <$> authors
    where
    authors = map strip $ splitOn "," $ resultAuthor sr
contextQueryToQuery (QueryPackageByAuthor)     _ = error "contextQueryToQuery: QueryPackageByAuthor: no package"
contextQueryToQuery (QueryModuleContent)       sr@NonPackageResult{} = (mkContext "package" $ getSRPackage sr) `qAnd` (modulesInContext "module" sr)
contextQueryToQuery (QueryModuleContent)       _ = error "contextQueryToQuery: QueryModuleContent: package"
contextQueryToQuery (QueryPackage)             sr@NonPackageResult{} = (mkContext "name" $ getSRPackage sr) `qAnd` (mkContext "type" "package")
contextQueryToQuery (QueryPackage)             _ = error "contextQueryToQuery: QueryPackage: package"
contextQueryToQuery (QueryModule)              sr@NonPackageResult{} = (modulesInContext "name" sr) `qAnd` (mkContext "type" "module")
contextQueryToQuery (QueryModule)              _ = error "contextQueryToQuery: QueryPackage: package"

contextQueryName :: ContextQuery -> Text
contextQueryName QueryReverseDependencies = "Reverse Dependencies"
contextQueryName QueryPackageModules = "Package Modules"
contextQueryName QueryPackageDatatypes = "Data types"
contextQueryName QueryPackageByAuthor = "Packages by same author"
contextQueryName QueryModuleContent = "Module content"
contextQueryName QueryPackage = "Show related package"
contextQueryName QueryModule = "Show related module"

contextQueries :: SearchResult -> [ContextQuery]
contextQueries sr
    | (resultType sr) == Package = [QueryReverseDependencies, QueryPackageModules, QueryPackageDatatypes, QueryPackageByAuthor]
    | (resultType sr) == Module  = [QueryModuleContent, QueryPackage]
    | otherwise                  = [QueryModule, QueryPackage]


mkContext :: Context -> Text -> Query
mkContext c w = QContext [c] $ QWord QCase w

modulesInContext :: Context -> SearchResult -> Query
modulesInContext c sr = (withinContexts [c] $ qOrs $ qWord <$> (resultModules sr))
-- ---------------------

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

-- -------------------------------

