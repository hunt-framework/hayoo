{-# LANGUAGE NoMonomorphismRestriction #-}

module MainCabal
where

import Control.Monad
import qualified Data.Map as Map
import ProcessCabal (processCabals, toPkgInfo, emitPkgInfo)
import TarUtil (ParsedEntry(..), cabalsInArchive, latestVersions)
import PackageInfo
import Hayoo.PackageRank (rankingStd)

import Text.Show.Pretty (ppShow)
import Pipes
import qualified Pipes.Prelude as P
import Pipes.Parse
import Pipes.Group
import Data.List
import Control.Monad.State.Strict
import qualified Control.Monad.State.Strict as S

import Data.Time.Clock (getCurrentTime)
import System.IO

import qualified JsonUtil as J
import qualified PkgIndexerCore as PC
import qualified Data.Aeson as A

ignore pkgInfo = return ()

-- | Just count the number of packages (latest versions only)
main1 = do
  (dag, count) <- processCabals ignore "index.tar.gz"
  putStrLn $ "count: " ++ show count

-- | Compute and display the rankings.
main2 = do
  (dag, count) <- processCabals ignore "index.tar.gz"
  let ranks = rankingStd dag
  forM_ (Map.assocs ranks) $ \(k,v) -> do
    putStrLn $ k ++ " " ++ show v

-- | Emit the package info records
main3 = do
  cabals <- cabalsInArchive "index.tar.gz"
  runEffect $ latestVersions cabals >-> for cat toPkgInfo >-> for cat showit
  where
    showit pkgInfo = lift $ putStrLn (ppShow pkgInfo)

-- | Print out packages which begin with "bytestring"
main4 = do
  cabals <- cabalsInArchive "index.tar.gz"
  runEffect $ cabals >-> P.filter go  >-> for cat (lift . print . fst)
  where
    go (pkgName, parsedEntry) = isPrefixOf "bytestring" pkgName

-- | Print out latest packages which begin with "bytestring"
main5 = do
  cabals <- cabalsInArchive "index.tar.gz"
  runEffect $ latestVersions (cabals >-> P.filter go)  >-> for cat (lift . display)
  where
    go (pkgName, parsedEntry) = isPrefixOf "bytestring" pkgName
    display (pkgName, parsedEntry) = putStrLn $ pkgName ++ " " ++ show (pe_version parsedEntry)

main6 = do
  cabals <- cabalsInArchive "index.tar.gz"
  let body = cabals >-> for cat (liftIO . print)
  runStateT (runEffect body) 10

main7 = do
  cabals <- cabalsInArchive "index.tar.gz"
  let body = cabals >-> counter >-> P.drain
  (_, n) <- runStateT (runEffect body) 10
  print n  -- number of cabal files seen
  where
    counter = forever doit
      where
        doit = do
            x <- await
            lift $ S.modify (+1)
            r <- lift S.get
            yield r

-- Update count and drain pipe
count = for cat (const $ lift $ modify (+1))

-- Count the number of cabal files
main8 = do
  cabals <- cabalsInArchive "index.tar.gz"
  let body = cabals >-> count
  (_, n) <- runStateT (runEffect body) 0
  putStrLn $ "cabal file count: " ++ show n

-- Count the number of packages
main9 = do
  cabals <- cabalsInArchive "index.tar.gz"
  let body = latestVersions cabals >-> count
  (_, n) <- runStateT (runEffect body) 0
  putStrLn $ "package count: " ++ show n

-- Add up the number of depdendencies in all packages.
main10 = do
  cabals <- cabalsInArchive "index.tar.gz"
  let body = latestVersions cabals
                >-> for cat toPkgInfo
                >-> for cat (\pkgInfo -> S.modify (+ (length (p_dependencies pkgInfo))))
  (_, n) <- runStateT (runEffect body) 0
  putStrLn $ "total dependency count: " ++ show n

-- Same as main10 but only take the first 10 packages
main11 = do
  cabals <- cabalsInArchive "index.tar.gz"
  let body = latestVersions cabals
                >-> P.take 10
                >-> for cat toPkgInfo
                >-> for cat (\pkgInfo -> S.modify (+ (length (p_dependencies pkgInfo))))
  (_, n) <- runStateT (runEffect body) 0
  putStrLn $ "total dependency count: " ++ show n

-- Collect the dependencies from each cabal file into a DAG.
main12 = do
  cabals <- cabalsInArchive "index.tar.gz"
  let pkgs = latestVersions cabals >-> P.take 10 >-> for cat toPkgInfo
      go dag pkgInfo = do
        let pkgName = p_name pkgInfo
            depends = p_dependencies pkgInfo
        return ((pkgName, depends):dag)
  dag <- runEffect $ P.foldM go (return []) return pkgs
  forM_ dag print

-- For each cabal file, emit the insert command as JSON and collect its dependencies into a DAG.
-- Crete the ranksings from the DAG and emit as 02-ranking.js
main13 = do
  now <- getCurrentTime
  cabals <- cabalsInArchive "index.tar.gz"
  let pkgs = latestVersions cabals >-> for cat toPkgInfo
      go dag pkgInfo = do
        let pkgName = p_name pkgInfo
            depends = p_dependencies pkgInfo
        lift $ emitPkgInfo now "json/01-pkg-" pkgInfo
        return ((pkgName, depends):dag)
  dag <- runEffect $ P.foldM go (return []) return pkgs

  -- compute ranks and emit 01-deletes.js and 02-ranking.js
  let ranks = rankingStd dag
      deletes = PC.buildDeletes (map fst (Map.assocs ranks))
  J.outputValue "json/01-deletes.js" deletes

  withBinaryFile "json/02-ranking.js" WriteMode $ \h -> do
    let cmds = [ J.buildUpdateWeight uri w | (pkgName, w) <- Map.assocs ranks,
                                             let uri = "http://hackage.haskell.org/package/" ++ pkgName ]
    J.emitJsonList h cmds

-- Same as main13 except create a single 01-packages.js file.
main indexTarPath = do
  now <- getCurrentTime
  cabals <- cabalsInArchive indexTarPath
  let pkgs = latestVersions cabals >-> for cat toPkgInfo
      go (dag,pkgs) pkgInfo = do
        let pkgName = p_name pkgInfo
            depends = p_dependencies pkgInfo
        return ((pkgName, depends):dag, pkgInfo:pkgs)
  (dag, plist) <- runEffect $ P.foldM go (return ([],[])) return pkgs

  let ranks = rankingStd dag
      pkgnames = map p_name plist
      deletes = PC.buildDeletes pkgnames
      inserts = map (PC.buildInsert now) plist

  -- emit 01-packages.js file
  J.outputValue "json/01-packages.js" $ A.toJSON (deletes:inserts)
  putStrLn "wrote json/01-packages.hs"

  -- emit 02-ranking.js:
  let updates = [ J.buildUpdateWeight uri w | (pkgName, w) <- Map.assocs ranks,
                                              let uri = "http://hackage.haskell.org/package/" ++ pkgName ]
  J.outputValue "json/02-ranking.js" $ A.toJSON updates
  putStrLn "wrote json/02-ranking.js"

{- another approach which perhaps avoids creating large Vectors in memory:

  withBinaryFile "json/01-packages.js" WriteMode $ \h ->
    J.emitJsonList h (deletes:inserts)
  withBinaryFile "json/02-ranking.js" WriteMode $ \h ->
    J.emitJsonList h updtes
-}

