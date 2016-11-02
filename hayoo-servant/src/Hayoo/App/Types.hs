{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Hayoo.App.Types
  ( ResultType (..)
  , PackageVersionResult (..)
  , SearchResult (..)
  ) where


import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char        (toLower)
import           Data.Scientific
import qualified Data.Text        as T
import           GHC.Generics
import qualified Text.Read        as TR


-- TYPES

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


newtype PackageVersionResult
  = PackageVersionResult T.Text


data SearchResult
  = NonPackageResult
    { resultScore       :: Scientific,
      resultUri         :: T.Text,
      resultPackage     :: T.Text,
      resultModules     :: [T.Text],
      resultName        :: T.Text,
      resultSignature   :: T.Text,
      resultDescription :: T.Text,
      resultSource      :: T.Text,
      resultType        :: ResultType
    }
  | PackageResult
    { resultScore        :: Scientific,
      resultUri          :: T.Text,
      resultName         :: T.Text,
      resultDependencies :: [T.Text],
      resultMaintainer   :: T.Text,
      resultSynopsis     :: T.Text,
      resultAuthor       :: T.Text,
      resultCategory     :: T.Text,
      resultType         :: ResultType
    }
  deriving (Show, Eq, Generic)


-- INSTANCES

instance FromJSON ResultType where
    parseJSON = genericParseJSON lowercaseConstructorsOptions

instance ToJSON ResultType where
    toJSON = genericToJSON lowercaseConstructorsOptions


instance FromJSON PackageVersionResult where
  parseJSON (Object o)
    = do (Object descr) <- o .: "description"
         PackageVersionResult <$> descr .: "version"
  parseJSON _
    = fail "packageVersionResult: expected Object"


instance ToJSON SearchResult

instance FromJSON SearchResult where
  parseJSON o = parseSearchResult o
    where
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

      parsePackageResult :: Scientific -> Object -> T.Text -> T.Text -> Parser SearchResult
      parsePackageResult score descr baseUri n = do
        dep <- descr .:? "dependencies" .!= []
        m   <- descr .:? "maintainer"   .!= ""
        s   <- descr .:? "synopsis"     .!= ""
        a   <- descr .:? "author"       .!= ""
        cat <- descr .:? "category"     .!= ""
        --    u <- baseUri -- unparsedUri
        return $ PackageResult score baseUri n dep m s a cat Package

      parseNonPackageResult :: Scientific -> Object -> T.Text -> T.Text -> T.Text -> ResultType
                            -> Parser SearchResult
      parseNonPackageResult score descr baseUri n d t = do
        p  <- descr .:? "package"   .!= "unknown"
        m  <- descr .:? "module"    .!= "[]"
        s  <- descr .:? "signature" .!= ""
        c  <- descr .:? "source"    .!= ""
    --    u <- baseUri -- unparsedUri
        let mods :: [T.Text]
            mods = maybe [m] id $ TR.readMaybe $ T.unpack m
        return $ NonPackageResult score baseUri p mods n s d c t


-- HELPERS

lowercaseConstructorsOptions :: Options
lowercaseConstructorsOptions =
  defaultOptions { constructorTagModifier = map toLower }
