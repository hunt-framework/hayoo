{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE BangPatterns #-}

module TestHoogle
where

import           Control.Monad
import           Pipes
import qualified Pipes.Prelude        as P
import qualified Pipes.Lift           as PL
import qualified Data.Set             as Set
import           Text.Show.Pretty
import           ProcessHoogle
import           ProcessLine          (fixupSignature)

import           Text.Parsec
import           ParseHoogle          (HoogleLine(..), hoogleLine)
import           Hayoo.ParseSignature
import qualified ProcessLine
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Control.Monad.State.Strict

import           Data.Time
import           Data.Either
import qualified Data.Map             as M
import           System.IO
import           System.FilePath
import           Hayoo.FunctionInfo   (FunctionInfo(..))
import           FctIndexerCore       (buildDelete)
import           JsonUtil             (hJsonPutStr, buildNOOP)

-- emit the FunctionInfo records in a Hoogle file
testFunctionInfo path = evalHState $ textLines path >-> toHoogleLine >-> toFunctionInfo >-> ppShowPipe

-- run parseSignature on the function declarations in a file
-- e.g. testParseSignatures "in-funcions"
testParseSignature path = runEffect $ (textLines path >-> toHoogleLine) `for` (lift . checkSig)

-- report on the number of unparsable signatures in a file
testCountBadSignatures path = do
  let accum (!bad,!total) (dbad, dtotal) = (bad+dbad, total+dtotal)
  (bad,total) <- P.fold accum (0,0) id $ textLines path >-> toHoogleLine >-> P.map checkSig'
  putStrLn $ path ++ ": " ++ show bad ++ " / " ++ show total ++ " unparsable signatures"

-- same as testCountBadSignatures but intermittently report the current counts
testCountBadSignatures' path = do
  let report (bad,total) = putStrLn $ path ++ ": " ++ show bad ++ " / " ++ show total ++ " unparsable signatures"
      accum (!bad,!total) (dbad, dtotal) = do
        let r = (bad+dbad, total+dtotal)
        when (mod (snd r) 10000 == 0) $ liftIO $ report r
        return r
  r <- P.foldM accum (return (0,0)) return $ textLines path >-> toHoogleLine >-> P.map checkSig'
  report r

-- Run parseSignature against a function signature
checkSig (FunctionDecl name sig) = do
  let sig' = fixupSignature sig
  case parseSignature sig' of
    Left e -> do putStrLn $ "error parsing signature: " ++ show e
                 putStrLn $ "  - name: " ++ name
                 putStrLn $ "  - sig : " ++ sig'
                 putStrLn ""
    Right s -> return ()
checkSig _ = return ()

-- returns (delta bad, delta total)
checkSig' :: HoogleLine -> (Int,Int)
checkSig' (FunctionDecl name sig) =
  let sig' = fixupSignature sig in
  either (const (1,1)) (const (0,1)) (parseSignature sig')
checkSig' _ = (0,0)

-- check a line against a parser
checkLine path parser (i,x) = do
  let source = path ++ " line " ++ show i
      str = T.unpack x
  case parse parser source str of
    Left e  -> do putStrLn $ "error: " ++ show e
                  T.putStr x
                  putStrLn ""
    Right _ -> return ()

-- test a parser against the lines in a file - do not skip preamble
testFile' parser path = do
  runEffect $ textLines path >-> forever (await >>= liftIO . checkLine path parser)

-- test a parser against the lines in a file - skip lines before @package
testFile parser path =
  runEffect $ skipHeader path >-> forever (await >>= liftIO . checkLine path parser)

-- test the anyLine parse against all of the hoogle files
testAllFiles = do
  files <- fmap lines $ readFile "all-hoogle-files"
  mapM_ (testFile hoogleLine) files

-- emit to stdout the Json for a hoogle file
testJson path = do
  now <- getCurrentTime
  evalHState $ skipHeader path >-> toHoogleLine >-> toFunctionInfo >-> toCommands (const $ Just 3.14) now >-> emitJsonStdout

processHoogleFiles scorePath emitDeleteCmd paths = do
  Just scoreMap <- readScores scorePath
  now <- getCurrentTime
  let scoreFn = \pkgName -> M.lookup pkgName scoreMap 
  forM_ paths $ \path -> do
    putStrLn $ "processing " ++ path
    processHoogleFile scoreFn now emitDeleteCmd path

-- filter out FunctionInfo records with the same docURI.
removeDupURIs = PL.evalStateP Set.empty (for cat go)
  where go item@(name, fi) = do
          seen <- get
          let uri = docURI fi
          if Set.member uri seen
            then return ()
            else do put (Set.insert uri seen)
                    yield item

processHoogleFile scoreFn now emitDeleteCmd path = do
  let pkgName = dropExtension $ takeBaseName path
      jsonPath = "json/" ++ pkgName ++ ".js"
  fh <- openFile jsonPath WriteMode 
  hPutStrLn fh "["
  if emitDeleteCmd
    then hJsonPutStr True fh (buildDelete pkgName)
    else hJsonPutStr True fh buildNOOP
  evalHState $ skipHeader path
                 >-> toHoogleLine 
                 >-> toFunctionInfo
                 >-> removeDupURIs
                 >-> toCommands scoreFn now
                 >-> emitCommaJson fh
  hPutStrLn fh "]"

