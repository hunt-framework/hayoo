{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable, MultiWayIf #-}

module TarUtil
  ( ParsedEntry(..)
  , TarException(..)
  , cabalsInArchive
  , latestVersions
  , pipesTarEntries
  , tarEntriesForPath
  )
where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Data.ByteString.Lazy as LBS

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.BZip as BZip

import Data.List (isSuffixOf, unfoldr)
import qualified Data.Version as V

import Text.ParserCombinators.ReadP

import Control.Monad
import Control.Exception
import Data.Typeable

import Pipes
import Pipes.Group
import qualified Pipes.Prelude as P
import Control.Lens (view)

data ParsedEntry = ParsedEntry { pe_package   :: String
                               , pe_version   :: V.Version
                               , pe_cabalname :: String
                               , pe_content   :: LBS.ByteString
                               , pe_size      :: Tar.FileSize
                               }
  deriving (Show)

data TarException = TarFormatError Tar.FormatError
  deriving (Show, Typeable)

instance Exception TarException where

tarEntriesForPath path = do
  let decompress
        | isSuffixOf ".bz2" path = BZip.decompress
        | isSuffixOf ".gz" path  = GZip.decompress
        | isSuffixOf ".tgz" path = GZip.decompress
        | otherwise              = id
  fmap (Tar.read . decompress) $ LBS.readFile path

pathParts path = unfoldr go path
  where go [] = Nothing
        go p  = case span (/= '/') p of
                 ([], [])     -> Nothing
                 ([], rest)   -> go (tail rest)
                 (leaf, rest) -> Just (leaf, rest)

hasThreeParts path =
 case pathParts path of
   (pkg : version : cabalname : _) -> Just (pkg, version, cabalname)
   _                               -> Nothing

firstParse :: ReadP a -> String -> Maybe a
firstParse readp str =
  case (readP_to_S readp) str of
    []        -> Nothing
    ((v,_):_) -> Just v

parseVersion :: ReadP V.Version
parseVersion = do v <- V.parseVersion; eof; return v

parsePath :: String -> Maybe (String, V.Version, String)
parsePath path = do
  (pkg, vers, cabal) <- hasThreeParts path
  version <- firstParse parseVersion vers
  return $ (pkg, version, cabal)

parseCabalEntry :: Tar.Entry -> Maybe ParsedEntry
parseCabalEntry ent = do
  (content, len) <- getNormalFileContent $ Tar.entryContent ent
  (pkg, vers, cabalname) <- hasThreeParts $ Tar.entryPath ent
  version <- firstParse parseVersion vers
  return $ ParsedEntry pkg version cabalname content len
  where
    getNormalFileContent (Tar.NormalFile content len) = Just (content,len)
    getNormalFileContent _                            = Nothing

-- | Fold consecutive pairs which have the same first component.
foldValues :: (Monad m, Eq k) => (v -> v -> v) -> Producer (k, v) m r -> Producer (k, v) m r
foldValues append xs =
    P.concat <-< folds step Nothing id (view (groupsBy keyEq) xs)
  where
    keyEq (k, _) (k', _) = k == k'

    step (Nothing)      (k, v) = Just (k, v)
    step (Just (_, v0)) (k, v) = Just (k, v0 `append` v)

-- | Create a stream of Tar.Entry.
pipesTarEntries :: Monad m => Tar.Entries Tar.FormatError -> Producer Tar.Entry m () 
pipesTarEntries entries = 
  case entries of
    Tar.Next ent next -> do yield ent; pipesTarEntries next
    Tar.Done          -> return ()
    Tar.Fail e        -> throw (TarFormatError e)

-- | Filter a stream of Tar.Entry to only include the cabal files.
--   Emit a stream of (pkgName, pkgEntry) pairs.
pipesSelectCabals :: Monad m => Pipes.Proxy () Tar.Entry () (String, ParsedEntry) m b
pipesSelectCabals = forever $ do
  ent <- await
  case parseCabalEntry ent of
     Nothing -> return ()
     Just r  -> yield (pe_package r, r)

-- | Open a tar archive and return a stream of (String, ParsedEntry)
--   of the .cabal files in the archive.
cabalsInArchive path = do
  entries <- tarEntriesForPath path 
  return $ pipesTarEntries entries >-> pipesSelectCabals

-- | Select only the latest versions in a stream of (String, ParsedEntry)
latestVersions p = foldValues cmp p
  where cmp old new = if (pe_version old < pe_version new) then new else old

-- | Return a Producer of the latest packages in an index.tar.gz file.
--   Assume that the packages names appear in sorted order.
pipesLatestPackages :: Monad m => FilePath -> IO (Producer (String, ParsedEntry) m ())
pipesLatestPackages path = do
  cabals <- cabalsInArchive path
  return $ latestVersions  cabals

-- display the latest versions of each package in an index.tar.gz file
test1 path = do
  p <- pipesLatestPackages path
  runEffect $ p >-> for cat (lift . showVersion)
  where
    showVersion (pkgName, pkgEnt) = putStrLn $ pkgName ++ " " ++ show (pe_version pkgEnt)

