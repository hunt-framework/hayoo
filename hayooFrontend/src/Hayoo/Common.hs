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

import           Control.Applicative         (Applicative, (<$>))
import           Control.Exception           (Exception, throwIO)
import           Control.Exception.Lifted    (Handler (..), catches)
--import           Control.Failure (Failure, failure)
import           Control.Monad.Base          (MonadBase, liftBase,
                                              liftBaseDefault)
import           Control.Monad.Catch         (MonadThrow)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
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
import           Data.String.Conversions     (cs, (<>))
import           Data.Text                   (Text, isInfixOf, replace, splitOn,
                                              strip)
import qualified Data.Text                   as Text
import           Data.Typeable               (Typeable)
--import           Data.Vector ((!))

import           Hunt.ClientInterface        (Context, Query, qAnd, qAnds,
                                              qContext, qOr, qOrs, qPhrase,
                                              qWord, qWordNoCase, qFullWord,
                                              setContexts, setBoost)
import qualified Hunt.ClientInterface        as H
import qualified Hunt.Server.Client          as H

import           Hunt.Query.Language.Parser  (parseQuery)

import           Network.HTTP.Conduit        (HttpException)

import qualified System.Log.Logger           as Log (debugM)

import           Text.Parsec                 (ParseError)
import           Text.Read                   (readMaybe)

import qualified Web.Scotty.Trans            as Scotty

import           Hayoo.ParseSignature

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
    s  <- descr .:? "signature" .!= ""
    c  <- descr .:? "source"    .!= ""
--    u <- baseUri -- unparsedUri
    let mods :: [Text]
        mods = maybe [m] id (readMaybe $ cs m)
    return $ NonPackageResult score  baseUri p mods n s d c t

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

#if MIN_VERSION_monad_control(1,0,0)
-- from http://hackage.haskell.org/package/monad-control-1.0.0.4/docs/src/Control-Monad-Trans-Control.html
instance MonadTransControl (HayooServerT) where
    type StT (HayooServerT) a = StT (ReaderT H.ServerAndManager) a
    liftWith = defaultLiftWith HayooServerT runHayooServerT
    restoreT = defaultRestoreT HayooServerT

-- From http://hackage.haskell.org/package/monad-control-1.0.0.4/docs/src/Control-Monad-Trans-Control.html
instance (MonadBaseControl b m) => MonadBaseControl b (HayooServerT m) where
    type StM (HayooServerT m) a = ComposeSt (HayooServerT) m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM
#else
-- from http://hackage.haskell.org/package/monad-control-0.3.2.3/docs/src/Control-Monad-Trans-Control.html
instance MonadTransControl (HayooServerT) where
    newtype StT (HayooServerT) a = StHayooServerT {unStHayooServerT :: StT (ReaderT H.ServerAndManager) a }
    liftWith = defaultLiftWith HayooServerT runHayooServerT StHayooServerT
    restoreT = defaultRestoreT HayooServerT unStHayooServerT

-- from http://hackage.haskell.org/package/monad-control-0.3.2.3/docs/src/Control-Monad-Trans-Control.html
instance (MonadBaseControl b m) => MonadBaseControl b (HayooServerT m) where
    newtype StM (HayooServerT m) a = StMHayooServer {unStMHayooServer :: ComposeSt (HayooServerT) m a}
    liftBaseWith = defaultLiftBaseWith StMHayooServer
    restoreM     = defaultRestoreM   unStMHayooServer
#endif

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


-- ------------------------------------------------------------
--
-- Query handling

handleSignatureQuery :: (Monad m, MonadIO m) => Text -> m Query
handleSignatureQuery q
  = do liftIO $ Log.debugM modName ("handleSignature in=" <> (cs q) <> ", out=" <> show qu)
       return qu
  where
    qu :: Query
    qu = qOrs $ concat [stdq, sigq, defq]

    isSig = isSignatureQuery q
    
    sig :: [Signature]
    sig = ( if isSig
            then id
            else complexSignatures 3
          )                            -- throw away too simple queries
          . either (const []) ((:[]))  -- throw away parser errors
          . parseNormSignature         -- try to parse q as signature
          . cs
          $ removeQuotes q

    subSigs :: [Signature]
    subSigs = concatMap (complexSignatures 1 . subSignatures) sig

    subSigq :: [Query]
    subSigq
      | null subSigs = []
      | otherwise    = (:[])
                       . setBoost 0.1    -- results of sub signature queries have a reduced score
                       . setContexts ["subsig"]
                       . qAnds
                       . map ( qFullWord  -- exact case sensitive word search
                               . cs       -- sub signatures must be found in sub signature context
                               . prettySignature
                             )
                       $ subSigs
                         
    sig1q :: [Query]
    sig1q = map ( setContexts ["signature"]
                  . qWord             -- case sensitive prefix search
                  . cs                -- convert to Text
                  . prettySignature   -- convert Signature into String
                ) sig

    sigq :: [Query]
    sigq = map (\ q' -> qOrs $ q' : subSigq) sig1q
      
    stdq :: [Query]
    stdq
      | isSig     = []
      | otherwise = either (const []) (:[])   -- throw away errors
                    $ parseQuery (cs q)       -- try to parse q as hunt query

    defq :: [Query]
    defq
      | null sigq                          -- if both query parsers fail,
        &&
        null stdq = [ qAnds                -- build a default query (AND)
                      . map qWordNoCase
                      . map removeQuotes
                      $ Text.words q       -- from the list of words
                    ]
      | otherwise = []

removeQuotes :: Text -> Text
removeQuotes t
  | Text.null t        = t
  | Text.head t == '"'
    &&
    Text.last t == '"' = Text.dropAround (== '"') t
  | Text.head t == '\''
    &&
    Text.last t == '\'' = Text.dropAround (== '\'') t
  | otherwise           = t
                          
isSignatureQuery :: Text -> Bool
isSignatureQuery q
  = "->" `isInfixOf` q
    ||
    "=>" `isInfixOf` q

{-
handleSignatureQuery :: (Monad m, MonadIO m) => Text -> m Query
handleSignatureQuery q
    | isSignatureQuery q = do
        s <- sig
        liftIO $ Log.debugM modName ("Signature Query: " <> (cs q) <> " >>>>>> " <> (show s))
        return s

    | otherwise = normalQuery
        where
        sig = case parseSignature $ cs q of
            (Right s) -> return sigQ
                where
                    q1 = ["signature"]  `setContexts` (qWord $ cs $ prettySignature s)
                    q2 = ["normalized"] `setContexts` (qWord $ cs $ prettySignature $ fst $ normalizeSignature s)
                    sigQ = qOr q2 q1
            (Left err) -> liftIO $ throwIO $ ParseError err

        normalQuery = case parseQuery (cs q) of
            (Right q') -> return q'
            (Left err) -> liftIO $ throwIO $ StringException err
-- -}

handleSignatureCompletionResults :: Text -> Query -> [Text] -> [Text]
handleSignatureCompletionResults txt q comps
    | isSignatureQuery txt = comps
    | otherwise          = H.printQuery <$> H.completeQueries q comps


autocomplete :: Text -> HayooAction [Text]
autocomplete q = raiseExeptions $ do
    q' <- handleSignatureQuery q
    handleSignatureCompletionResults q q' <$> (withServerAndManager' $ H.postAutocomplete q')


query :: Text -> Int -> HayooAction (H.LimitedResult SearchResult)
query q p = raiseExeptions $ do
    q' <- handleSignatureQuery q
    withServerAndManager' $ H.postQuery q' (p * 20)

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

modName :: String
modName = "HayooFrontend"

-- -------------------------------

