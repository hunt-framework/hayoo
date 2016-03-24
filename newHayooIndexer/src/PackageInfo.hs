module PackageInfo
where

type Score = Float

data PackageInfo
    = PackageInfo
      { p_name         :: String               -- ^ The name of the package
      , p_version      :: String               -- ^ The latest package version
      , p_dependencies :: [String]             -- ^ The list of required packages
      , p_author       :: String               -- ^ The author
      , p_maintainer   :: String               -- ^ The maintainer
      , p_category     :: String               -- ^ The package category
      , p_homepage     :: String               -- ^ The home page
      , p_synopsis     :: String               -- ^ The synopsis
      , p_description  :: String               -- ^ The description of the package
      , p_uploaddate   :: String               -- ^ The upload date
      , p_rank         :: ! Score              -- ^ The ranking
      }
    deriving (Show, Eq)

defPackageRank :: Score
defPackageRank = 1.0

