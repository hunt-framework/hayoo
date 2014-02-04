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

import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO)

import Data.Text (Text)
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
        resultUri :: Text, 
        resultName :: Text,
        resultDependencies :: Text,
        resultMaintainer :: Text,
        resultSynopsis :: Text,
        resultAuthor :: Text,
        resultCategory :: Text,
        resultType :: ResultType
    }  deriving (Show, Eq)


instance FromJSON SearchResult where
    parseJSON (Object v) = do
        u <- v .: "uri" 
        (Object descr) <- v .: "desc" -- This is always succesful. (as of january 2014)
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
                return $ PackageResult u n dep m s a cat Package
            _       -> do
                p  <- descr .: "package"
                m  <- descr .: "module"
                s  <- descr .: "signature"
                c  <- descr .:? "source" .!= ""
                return $ NonPackageResult u p m n s d c t
    parseJSON _ = mzero

newtype HayooServer a = HayooServer { runHayooServer :: ReaderT H.ServerAndManager IO a }
    deriving (Monad, MonadIO, MonadReader (H.ServerAndManager))

hayooServer :: MonadTrans t => HayooServer a -> t HayooServer a
hayooServer = lift 

runHayooReader :: HayooServer a -> H.ServerAndManager -> IO a
runHayooReader = runReaderT . runHayooServer

withServerAndManager' :: H.HolumbusConnectionT IO b -> HayooServer b
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