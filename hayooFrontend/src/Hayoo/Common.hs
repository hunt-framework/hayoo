{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Hayoo.Common 
(
  ResultType (..)
, SearchResult (..)
, HayooServer (..)
, hayooServer
, runHayooReader
, autocomplete
, query
, HayooConfiguration (..)
) where

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.ByteString.Lazy (ByteString)
import           Data.String.Conversions (cs) -- , (<>))

import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO)

import Data.Map (Map, fromList)
import Data.Text (Text)
--import qualified Data.Text.Encoding as T (decodeUtf8)
import Data.Aeson

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import "mtl" Control.Monad.Reader (ReaderT, MonadReader, ask, runReaderT)

import qualified Hunt.Server.Client as H

data ResultType = Class | Data | Function | Method | Module | Newtype | Package | Type | Unknown
    deriving (Eq, Show, Generic)


instance FromJSON ResultType where
     parseJSON = genericParseJSON H.lowercaseConstructorsOptions


data SearchResult =
    NonPackageResult {
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

instance FromJSON SearchResult where
    parseJSON (Object v) = do
        
        (Object descr) <- v .: "desc" -- This is always succesful. (as of january 2014)
        unparsedUri <- descr .: "uri" 
        
        n <- descr .:? "name" .!= ""
        d <- descr .:? "description" .!= ""
        t <- descr .:? "type" .!= Unknown
        case t of
            Package -> do
                dep <- descr .:? "dependencies" .!= ""
                m  <- descr .:? "maintainer" .!= ""
                s  <- descr .:? "synopsis" .!= ""
                a  <- descr .:? "author" .!= ""
                cat  <- descr .:? "category" .!= ""
                u <- parseUri m unparsedUri
                return $ PackageResult u n dep m s a cat Package
            _       -> do
                p  <- descr .:? "package" .!= "unknown"
                m  <- descr .:? "module" .!= "Unknown"
                s  <- descr .:? "signature" .!= ""
                c  <- descr .:? "source" .!= ""
                u <- parseUri p unparsedUri
                return $ NonPackageResult u p m n s d c t
    parseJSON _ = mzero



--instance FromJSON (Either Text (Map Text Text)) where
--    parseJSON (String v) = do

--    parseJSON (Object v) = do


newtype HayooServer a = HayooServer { runHayooServer :: ReaderT H.ServerAndManager IO a }
    deriving (Monad, MonadIO, MonadReader (H.ServerAndManager))

hayooServer :: MonadTrans t => HayooServer a -> t HayooServer a
hayooServer = lift 

runHayooReader :: HayooServer a -> H.ServerAndManager -> IO a
runHayooReader = runReaderT . runHayooServer

withServerAndManager' :: H.HuntConnectionT IO b -> HayooServer b
withServerAndManager' x = do
    sm <- ask
    --sm <- liftIO $ STM.readTVarIO var
    liftIO $ H.withServerAndManager x sm

autocomplete :: Text -> HayooServer (Either Text [Text])
autocomplete q = withServerAndManager' $ H.autocomplete q

query :: Text -> HayooServer (Either Text (H.LimitedResult SearchResult))
query q = withServerAndManager' $ H.query q

data HayooConfiguration = HayooConfiguration {
    hayooHost :: String, 
    hayooPort :: Int, 
    huntUrl :: String
} deriving (Show, Data, Typeable)