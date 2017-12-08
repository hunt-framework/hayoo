{-# LANGUAGE OverloadedStrings #-}
module Hayoo.Indexer.Cabal.Parse
  ( parse
  , parseErrorPretty
  ) where


import qualified Data.Text                             as T
import qualified Distribution.InstalledPackageInfo     as Cabal
import qualified Distribution.Package                  as C
import qualified Distribution.PackageDescription       as C
import qualified Distribution.PackageDescription.Parse as Cabal
import qualified Distribution.Version                  as C
import           Hayoo.Core.PackageInfo                (PackageInfo (..))



-- PARSE CABAL


parse :: T.Text -> Either Cabal.PError PackageInfo
parse input =
  case Cabal.parseGenericPackageDescription (T.unpack input) of
    Cabal.ParseFailed err ->
      Left err

    Cabal.ParseOk _warnings result ->
      pure $
        PackageInfo
          { pkgName =
              result
                |> C.packageDescription
                |> C.package
                |> C.pkgName
                |> C.unPackageName
                |> T.pack
          , pkgVersion =
              result
                |> C.packageDescription
                |> C.package
                |> C.pkgVersion
                |> C.versionNumbers
                |> map (T.pack . show)
                |> T.intercalate "."
          , pkgDependencies =
              result
                |> deps
          , pkgAuthor =
              result
                |> C.packageDescription
                |> C.author
                |> T.pack
          , pkgMaintainer =
              result
                |> C.packageDescription
                |> C.maintainer
                |> T.pack
          , pkgCategory =
              result
                |> C.packageDescription
                |> C.category
                |> T.pack
          , pkgHomepage =
              result
                |> C.packageDescription
                |> C.homepage
                |> T.pack
          , pkgSynopsis =
              result
                |> C.packageDescription
                |> C.synopsis
                |> T.pack
          , pkgDescription =
              result
                |> C.packageDescription
                |> C.description
                |> T.pack
          , pkgUploaddate   = ""
          , pkgRank         = 0
          }



-- DEPENDENCY RESOLUTION


deps :: C.GenericPackageDescription -> [T.Text]
deps desc =
  let
    edeps =
      concatMap (C.condTreeConstraints . snd) (C.condExecutables desc)

    ldeps =
      case C.condLibrary desc of
        Nothing ->
          []

        Just c ->
          C.condTreeConstraints c
  in
    (ldeps ++ edeps)
      |> map (T.pack . C.unPackageName . C.depPkgName)



parseErrorPretty :: Cabal.PError -> T.Text
parseErrorPretty err =
  case err of
    Cabal.AmbiguousParse msg line ->
      undefined

    Cabal.NoParse msg line ->
      undefined

    Cabal.TabsError line ->
      undefined

    Cabal.FromString str maybeLine ->
      undefined



-- HELPERS


(|>) :: a -> (a -> b) -> b
(|>) a f =
  f a
