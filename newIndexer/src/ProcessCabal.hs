{-# LANGUAGE BangPatterns #-}

module ProcessCabal
where

import qualified PkgIndexerCore as PC
import           PackageInfo (PackageInfo(..))
import           ParseCabal (parseCabal)
import           Pipes
import qualified Pipes.Prelude as P
import           TarUtil (ParsedEntry(..), cabalsInArchive, latestVersions)
import qualified Data.Text.Lazy.Encoding as LE
import qualified Data.Text.Lazy as LT
import qualified JsonUtil as JS

-- | Process the cabal files in an index.tar.gz file.
--   Returns the DAG and count of modules processed without any errors.
processCabals :: (PackageInfo -> IO ()) -> FilePath -> IO ([(String, [String])], Int)
processCabals emitCommand path = do
  cabals <- cabalsInArchive path
  runEffect $ P.foldM go (return ([], 1)) return (latestVersions cabals >-> for cat toPkgInfo)
  where
    go st@(dag, !n) pkgInfo = do
      liftIO $ emitCommand pkgInfo
      let pkgName = p_name pkgInfo
          depends = p_dependencies pkgInfo
      return ((pkgName, depends):dag, n+1)

-- | Convert a ParsedEntry to a PackageInfo
--   Note: This skips files with parse errors.
toPkgInfo (pkgName, pkgEntry) = do
  let cabalName = pe_cabalname pkgEntry
  case LE.decodeUtf8' (pe_content pkgEntry) of
    Left e     -> do liftIO $ putStrLn $ cabalName ++ ": utf8 decode error"
                     return ()
    Right text ->
      case parseCabal (LT.unpack text) of
        Nothing      -> do liftIO $ putStrLn $ cabalName ++ ": error parsing cabal file"
                           return ()
        Just pkgInfo -> yield pkgInfo

-- | Emit the command for a PackageInfo to a file.
emitPkgInfo now prefix pkgInfo = do
  let pkgName = p_name pkgInfo
      insertCommand = PC.buildInsert now pkgInfo
      path = prefix ++ pkgName ++ ".js"
  JS.outputValue path insertCommand

