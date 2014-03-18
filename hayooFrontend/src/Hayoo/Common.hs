{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Hayoo.Common 
(
  ResultType (..)
, SearchResult (..)
, HayooServer (..)
, hayooServer
, runHayooReader
, runHayooReader'
, autocomplete
, query
, HayooConfiguration (..)
) where

import           GHC.Generics (Generic)

-- import           Control.Monad (mzero)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Reader (ReaderT, MonadReader, ask, runReaderT)

import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Data (Data)
import           Data.Map (Map, fromList)
import           Data.String.Conversions (cs, (<>))
import           Data.Text (Text, isInfixOf)
import           Data.Typeable (Typeable)
import           Data.Vector ((!))

#if MIN_VERSION_aeson(0,7,0)
import           Data.Scientific (Scientific)
#else
import           Data.Attoparsec.Number (Number (D))
#endif

import qualified System.Log.Logger as Log (debugM)

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


instance FromJSON SearchResult where
    parseJSON (Array v) = do
#if MIN_VERSION_aeson(0,7,0)    
        let (Number rank) = v ! 1
#else
        let (Number (D rank)) = v ! 1
#endif
            o = v ! 0
        parseSearchResult rank o
    parseJSON _ = fail "Expected Tuple (Array) for SearchResult"



--instance FromJSON (Either Text (Map Text Text)) where
--    parseJSON (String v) = do

--    parseJSON (Object v) = do


newtype HayooServer a = HayooServer { runHayooServer :: ReaderT H.ServerAndManager IO a }
    deriving (Monad, MonadIO, MonadReader (H.ServerAndManager))

hayooServer :: MonadTrans t => HayooServer a -> t HayooServer a
hayooServer = lift 

runHayooReader :: HayooServer a -> H.ServerAndManager -> IO a
runHayooReader = runReaderT . runHayooServer

runHayooReader' :: HayooServer a -> Text -> IO a
runHayooReader' x s = do
    sm <- H.newServerAndManager s
    runHayooReader x sm

withServerAndManager' :: H.HuntConnectionT IO b -> HayooServer b
withServerAndManager' x = do
    sm <- ask
    --sm <- liftIO $ STM.readTVarIO var
    liftIO $ H.withServerAndManager x sm

-- ------------------------

-- TODO Error handling
handleSignatureQuery :: (Monad m, MonadIO m) => Text -> m (Either Text Query)
handleSignatureQuery q
    | "->" `isInfixOf` q = do
        s <- sig
        liftIO $ Log.debugM modName ("Signature Query: " <> (cs q) <> " >>>>>> " <> (show s))
        return $ Right s
    | otherwise = return $ Left q
    where
        -- sig = either (fail . show) (return . (<> "\"") . ("signature:\"" <>) . cs . prettySignature . fst . normalizeSignature) $ parseSignature $ cs q
        sig = either (fail . show) return $ do
            s <- parseSignature $ cs q
            let q1 = QContext ["signature"] $ QWord QCase (cs $ prettySignature s)
                q2 = QContext ["normalized"] $ QWord QCase (cs $ prettySignature $ fst $ normalizeSignature s)
                sigQ = QBinary Or q1 q2
            return sigQ

autocomplete :: Text -> HayooServer (Either Text [Text])
autocomplete q = do
    q' <- handleSignatureQuery q
    withServerAndManager' $ either H.autocomplete (H.evalAutocomplete q) $ q'

query :: Text -> HayooServer (Either Text (H.LimitedResult SearchResult))
query q = do
    q' <- handleSignatureQuery q
    withServerAndManager' $ either H.query H.evalQuery $ q'

data HayooConfiguration = HayooConfiguration {
    hayooHost :: String, 
    hayooPort :: Int, 
    huntUrl :: String
} deriving (Show, Data, Typeable)

modName :: String
modName = "HayooFrontend"