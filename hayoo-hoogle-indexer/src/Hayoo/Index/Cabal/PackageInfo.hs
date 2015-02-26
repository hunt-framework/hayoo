{-# LANGUAGE OverloadedStrings #-}
module Hayoo.Index.Cabal.PackageInfo where

import           Data.Aeson (object, Value(..), (.=), toJSON)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Version (showVersion)
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Configuration as Cabal
import qualified Distribution.PackageDescription.Parse as Cabal
import qualified Distribution.Text as Cabal
import           Hayoo.Index.IndexSchema
import qualified Hunt.ClientInterface as Hunt
import qualified Hunt.Common.DocDesc as Hunt

import Debug.Trace

type Error = String

data PackageDescription = PackageDescription {
    pdName         :: Text
  , pdVersion      :: Text
  , pdUri          :: Text
  , pdAuthor       :: Text
  , pdCategory     :: Text
  , pdDependencies :: [Text]
  , pdHomepage     :: Text
  , pdMaintainer   :: Text
  , pdSynopsis     :: Text
  , pdDescription  :: Text
  , pdModules      :: [Text]
  } deriving (Eq, Show)

instance Hunt.Huntable PackageDescription where
  huntURI      = pdUri
  huntDescr pd = Hunt.mkDocDesc o
    where
      Object o = object [
          d'author       .= pdAuthor pd
        , d'category     .= pdCategory pd
        , d'dependencies .= pdDependencies pd
        , d'synopsis     .= pdSynopsis pd
        , d'maintainer   .= pdMaintainer pd
        , d'name         .= pdName pd
        , d'version      .= pdVersion pd
        , d'type         .= ("package" :: Text)
        , d'modules      .= pdModules pd
        ]
  huntIndexMap = Map.fromList . toList

toList :: PackageDescription -> [(Text, Text)]
toList pd = filter (not . Text.null . snd) [
    (d'author, pdAuthor pd)
  , (d'category, pdCategory pd)
  , (d'dependencies, Text.intercalate " " (pdDependencies pd))
  , (d'synopsis, pdSynopsis pd)
  , (d'maintainer, pdMaintainer pd)
  , (d'name, pdName pd)
  , (d'version, pdVersion  pd)
  , (d'type, "package")
  , (d'modules, Text.intercalate " " (pdModules pd))
  ]

parseCabalFile :: (String -> String -> String) ->  String -> Either Error PackageDescription
parseCabalFile mkUri content =
  case parseDesc content of
   Cabal.ParseFailed err -> Left (show err)
   Cabal.ParseOk _ desc  -> Right desc
  where
    parseDesc desc = do
      cabalGenDesc <- Cabal.parsePackageDescription desc
      let cabalDesc = Cabal.flattenPackageDescription cabalGenDesc
      return (cabalDescToHayooDesc mkUri cabalDesc)

cabalDescToHayooDesc :: (String -> String -> String) -> Cabal.PackageDescription -> PackageDescription
cabalDescToHayooDesc mkUri desc =
  PackageDescription {
      pdName         = pkgName
    , pdVersion      = version
    , pdUri          = Text.pack $ mkUri (Cabal.display (Cabal.pkgName package)) (showVersion (Cabal.pkgVersion package))
    , pdAuthor       = Text.pack $ Cabal.author desc
    , pdCategory     = Text.pack $ Cabal.category desc
    , pdDependencies = dependencies (Cabal.buildDepends desc)
    , pdHomepage     = Text.pack $ Cabal.homepage desc
    , pdMaintainer   = Text.pack $ Cabal.maintainer desc
    , pdSynopsis     = Text.pack $ Cabal.synopsis desc
    , pdDescription  = Text.pack $ Cabal.description desc
    , pdModules      = maybe [] libModules (Cabal.library desc)
    }
  where
    package = Cabal.package desc
    pkgName = Text.pack $ Cabal.display (Cabal.pkgName package)
    version = Text.pack $ showVersion (Cabal.pkgVersion package)

dependencies :: [Cabal.Dependency] -> [Text]
dependencies = fmap Text.pack . fmap toPkg
  where
    toPkg (Cabal.Dependency pkg _) = Cabal.display pkg

libModules :: Cabal.Library -> [Text]
libModules = fmap (Text.pack . Cabal.display)
             . Cabal.libModules
