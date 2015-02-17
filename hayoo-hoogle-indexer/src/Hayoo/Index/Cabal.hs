module Hayoo.Index.Cabal where

import           Conduit
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import           Hayoo.Index.Cabal.PackageInfo
import           Hayoo.Index.Conduit
import           Hunt.ClientInterface
import           Hunt.Conduit
import           Hunt.Server.Client

type PackageName = String
type Version = String

type MkPackageUri = PackageName -> Version -> String

mkHackageUri :: String -> PackageName -> Version -> String
mkHackageUri base package version =
  base ++ "/package/" ++ package ++ "-" ++ version ++ "/"

indexCabalArchive :: MkPackageUri -> FilePath -> HuntConnectionT IO ()
indexCabalArchive mkUri fp = do
  archive <- liftIO $ ByteString.readFile fp
  compressedArchive archive
    =$= packageInfos mkUri
    =$= leftLogger "indexCabalArchive: "
    =$= makeInserts toApiDocument
    =$= rechunkCommands 50
    $$ cmdSink

packageInfos :: Monad m
             => MkPackageUri
             -> Conduit (FilePath, ByteString) m (Either Error PackageDescription)
packageInfos mkUri = mapC (parseCabalFile mkUri . utf8 . snd)
