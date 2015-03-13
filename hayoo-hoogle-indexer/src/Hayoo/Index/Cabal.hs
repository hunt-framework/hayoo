{-# LANGUAGE Rank2Types #-}
module Hayoo.Index.Cabal where

import           Hayoo.Index.Cabal.PackageInfo
import           Hayoo.Index.Conduit
import           Hayoo.Index.PackageRank

import           Conduit
import           Control.Monad.Primitive
import           Control.Monad.State.Strict
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import           Data.Either
import qualified Data.List as List
import           Data.Text (Text)
import           Hunt.ClientInterface
import           Hunt.Conduit
import           Hunt.Server.Client

type PackageName = String
type Version = String

type MkPackageUri = PackageName -> Version -> String

mkHackageUri :: String -> PackageName -> Version -> String
mkHackageUri base package version =
  base ++ "/package/" ++ package ++ "-" ++ version ++ "/"

-- | Process cabal files from index.tar in a streaming fashion
--   and build a dependency graph on-the-fly.
indexCabalArchive :: (MonadIO m, MonadBase b m, PrimMonad b)
                  =>  MkPackageUri
                  -> FilePath
                  -> Producer (StateT (DAGList Text) m) Command
indexCabalArchive mkUri fp = do
  archive <- liftIO $ ByteString.readFile fp
  compressedArchive archive
    =$= packageInfos mkUri
    =$= leftLogger "indexCabalArchive: "
    =$= makeInserts toApiDocument
    =$= rechunkCommands 50

packageInfos :: Monad m
             => MkPackageUri
             -> Conduit (FilePath, ByteString) (StateT (DAGList Text) m) (Either Error PackageDescription)
packageInfos mkUri = awaitForever $ \(_, content) -> do
  let cabal = parseCabalFile mkUri (utf8 content)
  when (isRight cabal) $ do
    let Right pd = cabal
        deps     = List.filter (pdName pd /=) (pdDependencies pd)
    lift $ modify ((pdName pd, deps) :)
  yield cabal