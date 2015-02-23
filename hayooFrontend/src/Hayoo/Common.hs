{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Hayoo.Common
  ( ResultType (..)
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
  , H.printQuery
  , contextQueryToQuery
  , contextQueryName
  , ContextQuery ()
  , contextQueries
  , escapeScript
  )
where

import           GHC.Generics                (Generic)

import           Control.Applicative
import           Control.Exception           (Exception)
import           Control.Exception.Lifted    (Handler (..), catches)
--import           Control.Failure (Failure, failure)
import           Control.Monad.Base          (MonadBase, liftBase,
                                              liftBaseDefault)
import           Control.Monad.Catch         (MonadThrow)
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Reader        (MonadReader, ReaderT, ask,
                                              runReaderT)
import           Control.Monad.Trans.Class   (MonadTrans, lift)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl,
                                              MonadTransControl, StM, StT,
                                              defaultLiftBaseWith,
                                              defaultLiftWith, defaultRestoreM,
                                              defaultRestoreT, liftBaseWith,
                                              liftWith, restoreM, restoreT)

import           Data.Aeson
import           Data.Aeson.Types            (Parser)
import           Data.Data                   (Data)
import           Data.Scientific             (Scientific)
import           Data.String                 (IsString, fromString)
import           Data.String.Conversions     (cs)
import           Data.Text                   (Text, replace, splitOn,
                                              strip)
import           Data.Typeable               (Typeable)
--import           Data.Vector ((!))

import           Hunt.ClientInterface        (Context, Query, qAnd,
                                              qContext, qOrs, qPhrase,
                                              qWord,
                                              setContexts)
import qualified Hunt.ClientInterface        as H
import qualified Hunt.Server.Client          as H


import           Network.HTTP.Conduit        (HttpException)

import           Text.Parsec                 (ParseError)
import           Text.Read                   (readMaybe)

import qualified Web.Scotty.Trans            as Scotty

import           Hayoo.Query

-- ------------------------------------------------------------

data ResultType
  = Class
  | Data
  | Function
  | Method
  | Module
  | Newtype
  | Package
  | Type
  | Unknown
  deriving (Eq, Show, Generic)

instance FromJSON ResultType where
    parseJSON = genericParseJSON H.lowercaseConstructorsOptions

instance ToJSON ResultType where
    toJSON = genericToJSON H.lowercaseConstructorsOptions


#if MIN_VERSION_aeson(0,7,0)
type AesonScore = Scientific
#else
type AesonScore = Double
#endif

data SearchResult
  = NonPackageResult
    { resultScore       :: AesonScore,
      resultUri         :: Text,
      resultPackage     :: Text,
      resultModules     :: [Text],
      resultName        :: Text,
      resultSignature   :: Text,
      resultDescription :: Text,
      resultSource      :: Text,
      resultType        :: ResultType
    }
  | PackageResult
    { resultScore        :: AesonScore,
      resultUri          :: Text,
      resultName         :: Text,
      resultDependencies :: [Text],
      resultMaintainer   :: Text,
      resultSynopsis     :: Text,
      resultAuthor       :: Text,
      resultCategory     :: Text,
      resultType         :: ResultType
    }
  deriving (Show, Eq, Generic)

getSRPackage :: SearchResult -> Text
getSRPackage sr@NonPackageResult{} = resultPackage sr
getSRPackage sr@PackageResult{} = resultName sr

escapeScript :: (SearchResult -> Text) -> SearchResult -> Text
escapeScript f sr =   replace "<script"  "&lt;script"
                    $ replace "</script" "&lt;/script"
                    $ f sr

parsePackageResult :: AesonScore -> Object -> Text -> Text -> Parser SearchResult
parsePackageResult score descr baseUri n = do
    dep <- descr .:? "dependencies" .!= []
    m   <- descr .:? "maintainer"   .!= ""
    s   <- descr .:? "synopsis"     .!= ""
    a   <- descr .:? "author"       .!= ""
    cat <- descr .:? "category"     .!= ""
--    u <- baseUri -- unparsedUri
    return $ PackageResult score baseUri n dep m s a cat Package

parseNonPackageResult :: AesonScore -> Object -> Text -> Text -> Text -> ResultType
                         -> Parser SearchResult
parseNonPackageResult score descr baseUri n d t = do
    p  <- descr .:? "package"   .!= "unknown"
    m  <- descr .:? "module"    .!= "[]"
    s  <- signature
    c  <- descr .:? "source"    .!= ""
--    u <- baseUri -- unparsedUri
    let mods :: [Text]
        mods = maybe [m] id (readMaybe $ cs m)
    return $ NonPackageResult score  baseUri p mods n s d c t
  where
    signature =
      descr .: "disp-signature" <|> descr .: "signature" <|> pure ""

parseSearchResult :: Value -> Parser SearchResult
parseSearchResult (Object v) = do
    (Object descr) <- v .: "description" -- This is always succesful. (as of january 2014)
    (Number score) <- v .:? "score" .!= (Number 1.0)
    baseUri        <- v .: "uri"
    -- unparsedUri <- descr

    n <- descr .:? "name"        .!= ""
    d <- descr .:? "description" .!= ""
    t <- descr .:? "type"        .!= Unknown
    case t of
        Package -> parsePackageResult    score descr baseUri n
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
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (H.ServerAndManager), MonadTrans, MonadThrow)

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
    H.withServerAndManager sm x

handleSignatureCompletionResults :: Text -> Query -> [Text] -> [Text]
handleSignatureCompletionResults txt q comps
    | isSignatureQuery txt = comps
    | otherwise            = H.printQuery <$> H.completeQueries q comps

autocomplete :: Text -> HayooAction [Text]
autocomplete q = raiseExeptions $ do
  handleSignatureCompletionResults q q' <$> withServerAndManager' (H.postAutocomplete q')
  where
    q' = hayooQuery q


query :: Text -> Int -> HayooAction (H.LimitedResult SearchResult)
query q p = raiseExeptions $ do
    withServerAndManager' $ H.postQuery (hayooQuery q) (p * 20)

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
contextQueryToQuery (QueryReverseDependencies) sr = qContext "dependencies" $ getSRPackage sr
contextQueryToQuery (QueryPackageModules)      sr = qAnd (qContext "package" $ getSRPackage sr) (qContext "type" "module")
contextQueryToQuery (QueryPackageDatatypes)    sr = qAnd (qContext "package" $ getSRPackage sr) (qOrs $ (qContext "type") <$> ["data", "newtype", "type"])
contextQueryToQuery (QueryPackageByAuthor)     sr@PackageResult{} = qOrs $ (setContexts ["author"] . qPhrase) <$> authors
    where
    authors = map strip $ splitOn "," $ resultAuthor sr
contextQueryToQuery (QueryPackageByAuthor)     _ = error "contextQueryToQuery: QueryPackageByAuthor: no package"
contextQueryToQuery (QueryModuleContent)       sr@NonPackageResult{} = (qContext "package" $ getSRPackage sr) `qAnd` (modulesInContext "module" sr)
contextQueryToQuery (QueryModuleContent)       _ = error "contextQueryToQuery: QueryModuleContent: package"
contextQueryToQuery (QueryPackage)             sr@NonPackageResult{} = (qContext "name" $ getSRPackage sr) `qAnd` (qContext "type" "package")
contextQueryToQuery (QueryPackage)             _ = error "contextQueryToQuery: QueryPackage: package"
contextQueryToQuery (QueryModule)              sr@NonPackageResult{} = (modulesInContext "module" sr) `qAnd` (qContext "type" "module")
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



modulesInContext :: Context -> SearchResult -> Query
modulesInContext c sr = (setContexts [c] $ qOrs $ qWord <$> (resultModules sr))

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
    huntUrl   :: String
} deriving (Show, Data, Typeable)


-- -------------------------------
