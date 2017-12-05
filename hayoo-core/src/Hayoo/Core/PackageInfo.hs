module Hayoo.Core.PackageInfo
  ( PackageInfo (..)
  , Score
  ) where


import qualified Data.Text as T



-- PACKAGE INFO


type Score
  = Float


-- | The PackageInfo represents information about a package.
data PackageInfo
    = PackageInfo
      { pkgName         :: T.Text   -- ^ The name of the package
      , pkgVersion      :: T.Text   -- ^ The latest package version
      , pkgDependencies :: T.Text   -- ^ The list of required packages
      , pkgAuthor       :: T.Text   -- ^ The author
      , pkgMaintainer   :: T.Text   -- ^ The maintainer
      , pkgCategory     :: T.Text   -- ^ The package category
      , pkgHomepage     :: T.Text   -- ^ The home page
      , pkgSynopsis     :: T.Text   -- ^ The synopsis
      , pkgDescription  :: T.Text   -- ^ The description of the package
      , pkgUploaddate   :: T.Text   -- ^ The upload date
      , pkgRank         :: !Score   -- ^ The ranking
      } deriving (Show, Eq)
