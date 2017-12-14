module Hayoo.Indexer
  ( Config (..)
  , Mode (..)
  , Statistics (..)
  , index
  ) where


import           Servant.Client (BaseUrl)



-- INDEXING


-- |
data Config
  = Config
    { outputDirectory :: FilePath
    , mode            :: Mode
    } deriving (Show)


-- | @Mode@ describes the way in which indexing happens.
--
--     * /HoogleFile:/ Means that only a simple single file will
--                     be indexed.
--     * /CabalFile:/ Means that only a simple, single cabal file
--                    will be indexed.
--     * /Automatic:/ Means, that a complete, automatic, new indexer run
--                    will be invoked, downloading necessary files and
--                    handling any other mangling.
--     * /Manual:/    Means, that a complete, new indexer run will be
--                    invoked, by using the provided Hoogle and Cabal
--                    files.
--
-- Both, Automatic and Manual indexing may take some time.
data Mode
  = HoogleFile
    { hoogleFile :: FilePath
    , withSchema :: Bool
    }
  | CabalFile
    { cabalFile  :: FilePath
    , withSchema :: Bool
    }
  | Automatic
  | Manual
    { hoogleTarArchive :: FilePath
    , cabalTarArchive  :: FilePath
    } deriving (Show)


-- | Keeps track of useful statistics throughout
-- indexing.
data Statistics
  = Statistics
    {
    } deriving (Show)


-- | @index@ hoogle and cabal files for @pump@ing them into
-- the hunt searchengine.
index :: Config -> IO Statistics
index config =
  undefined
