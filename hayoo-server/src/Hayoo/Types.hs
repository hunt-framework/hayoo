{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Hayoo.Types
  ( -- * Types
    ResultType (..)
  , PackageVersionResult (..)
  , SearchResult (..)

    -- * Operations
  , contextQueryToQuery
  , contextQueryName
  , contextQueries
  ) where


import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char            (toLower)
import           Data.Scientific
import qualified Data.Text            as T
import           GHC.Generics
import           Hunt.ClientInterface (Context, Query, qAnd, qContext, qOrs,
                                       qPhrase, qWord, setContexts)
import qualified Text.Read            as TR


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


data ContextQuery
    = QueryReverseDependencies
    | QueryPackageModules
    | QueryPackageDatatypes
    | QueryPackageByAuthor
    | QueryModuleContent
    | QueryPackage
    | QueryModule
    deriving (Show)


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


-- OPERATIONS

getSRPackage :: SearchResult -> T.Text
getSRPackage sr@NonPackageResult{} = resultPackage sr
getSRPackage sr@PackageResult{} = resultName sr


contextQueryToQuery :: ContextQuery -> SearchResult -> Query
contextQueryToQuery query result =
  case (query, result) of
    (QueryReverseDependencies, sr) ->
      qContext "dependencies" $ getSRPackage sr

    (QueryPackageModules, sr) ->
      (qContext "package" $ getSRPackage sr) `qAnd` (qContext "type" "module")

    (QueryPackageDatatypes, sr) ->
      let
        packageCtx = qContext "package" $ getSRPackage sr
        typeCtx    = qContext "type" <$> ["data", "newtype", "type"]
      in
        packageCtx `qAnd` (qOrs typeCtx)

    (QueryPackageByAuthor, sr@PackageResult{}) ->
      let
        authors = map T.strip $ T.splitOn "," $ resultAuthor sr
      in
        qOrs $ (setContexts ["author"] . qPhrase) <$> authors

    (QueryPackageByAuthor, _) ->
      error "contextQueryToQuery: QueryPackageByAuthor: no package"

    (QueryModuleContent, sr@NonPackageResult{}) ->
      (qContext "package" $ getSRPackage sr) `qAnd` (modulesInContext "module" sr)

    (QueryModuleContent, _) ->
      error "contextQueryToQuery: QueryModuleContent: package"

    (QueryPackage, sr@NonPackageResult{}) ->
      (qContext "name" $ getSRPackage sr) `qAnd` (qContext "type" "package")

    (QueryPackage, _) ->
      error "contextQueryToQuery: QueryPackage: package"

    (QueryModule, sr@NonPackageResult{}) ->
      (modulesInContext "module" sr) `qAnd` (qContext "type" "module")

    (QueryModule, _) ->
      error "contextQueryToQuery: QueryPackage: package"


contextQueryName :: ContextQuery -> T.Text
contextQueryName query =
  case query of
    QueryReverseDependencies ->
      "Reverse Dependencies"

    QueryPackageModules ->
      "Package Modules"

    QueryPackageDatatypes ->
      "Data types"

    QueryPackageByAuthor ->
      "Packages by same author"

    QueryModuleContent ->
      "Module content"

    QueryPackage ->
      "Show related package"

    QueryModule ->
      "Show related module"


contextQueries :: SearchResult -> [ContextQuery]
contextQueries sr =
  case resultType sr of
    Package ->
      [QueryReverseDependencies, QueryPackageModules, QueryPackageDatatypes, QueryPackageByAuthor]

    Module ->
      [QueryModuleContent, QueryPackage]

    _ ->
      [QueryModule, QueryPackage]


-- HELPERS

modulesInContext :: Context -> SearchResult -> Query
modulesInContext c sr = (setContexts [c] $ qOrs $ qWord <$> (resultModules sr))


lowercaseConstructorsOptions :: Options
lowercaseConstructorsOptions =
  defaultOptions { constructorTagModifier = map toLower }
