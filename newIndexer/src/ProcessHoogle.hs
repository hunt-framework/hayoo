{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module ProcessHoogle
where

import qualified Data.Aeson       as A

import qualified Data.Text        as Text
import qualified Data.Text.IO     as Text
import           Data.Text           (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy.Encoding as DTLE
import qualified Data.Text.Lazy as DTL

import           Pipes
import qualified Pipes.Prelude    as P
import           ParseHoogle      (hoogleLine)
import           Control.Monad
import           Text.Parsec

import qualified ProcessLine      as PL
import           Control.Monad.State.Strict

import qualified Text.Show.Pretty as PP

import           Hayoo.FunctionInfo (Score, FunctionInfo(..))

import qualified FctIndexerCore   as FC
import           Data.Time        (UTCTime, getCurrentTime)

import           JsonUtil         (jsonPutStr, hJsonPutStr)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map        as Map
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified System.FilePath.Posix as FP
import Control.Monad.Error

-- A producer which yields the lines of a file (as Text)
-- The entire file is read strictly, so there shouldn't be any resource cleanup issues.
-- XXX - make sure UTF-8 is used here.
textLines path = do
  lns <- liftIO $ fmap Text.lines (Text.readFile path)
  forM_ (zip [(1::Int)..] lns) $ yield

byteLines path = do
  lns <- liftIO $ fmap BS.lines (BS.readFile path)
  forM_ (zip [(1::Int)..] lns) $ yield

-- skip all lines before @package because they might not be well-formed UTF-8.
skipToPackage = do
  (i,x) <- await
  if (BS.isPrefixOf "@package" x)
    then do yield (i, Text.decodeUtf8 x)
            forever $ do { (i,x) <- await; yield (i, Text.decodeUtf8 x) }
    else skipToPackage

skipHeader path = byteLines path >-> skipToPackage

-- Skip the header on a lazy bytestring.
-- This is suitable for feeding into toHoogleLine.
skipHeaderLBS lbs =
  let pairs = zip [(1::Int)..] (LBS.lines lbs)
      rest = dropWhile (\(i,x) -> not (LBS.isPrefixOf "@package" x)) pairs
  in each $ map (\(i,x) -> (i, DTL.toStrict (DTLE.decodeUtf8 x))) rest

-- convert a Text line into a Hoogle line
toHoogleLine = forever $ do
  (lineno, txt)  <- await
  case parse hoogleLine "(file)" (Text.unpack txt) of
    Left e      -> liftIO $ do putStrLn $ "error on line " ++ show lineno ++ ": " ++ show e
    Right hline -> yield hline

-- Convert a HoogleLine to a (String, FunctionInfo) pair
toFunctionInfo = forever $ do
  hline <- await
  PL.processLine yield hline

-- Pretty-print each element in a stream
ppShowPipe = for cat $ liftIO . putStrLn . PP.ppShow

-- emit JSON to stdout
emitJsonStdout = for cat (liftIO . jsonPutStr True)

emitJson fh = for cat (liftIO . hJsonPutStr True fh)

emitCommaJson fh = for cat $ \x -> liftIO $ do { Text.hPutStrLn fh ","; hJsonPutStr True fh x }

-- Convert (fctName, fctInfo) to JSON commands
toCommands scoreFn now = for cat $ \item@(fctName, fctInfo) -> do
  let score = scoreFn (package fctInfo)
      cmds = FC.buildInserts score now [ item ]
  each cmds

-- Run a MonadState HState pipeline
evalHState pipeline = evalStateT (runEffect pipeline) PL.emptyHState

data UriWeight = UriWeight { docUri :: String, docWeight :: Score }

instance A.FromJSON UriWeight where
  parseJSON (A.Object v) = do d <- v A..: "document"
                              u <- d A..: "uri"
                              w <- d A..: "weight"
                              return $ UriWeight u w
  parseJSON _            = mzero

-- read scores from a JSON file; return a Map
readScores :: FilePath -> IO (Maybe (Map.Map String Score))
readScores path = do
  content <- LBS.readFile path
  case A.decode content of
    Nothing   ->  return Nothing
    Just cmds ->  let m = Map.fromList [ (FP.takeBaseName u, w) | UriWeight u w <- cmds ]
                  in return (Just m)

{-
test1pipe path = skipHeader path >-> toHoogleLine >-> toFunctionInfo >-> P.drain
test2pipe path = skipHeader path >-> toHoogleLine >-> toFunctionInfo >-> ppShowPipe
test1 = evalHState . test1pipe
test2 = evalHState . test2pipe
test3 path = do
  now <- getCurrentTime
  evalHState $ skipHeader path >-> toHoogleLine >-> toFunctionInfo >-> toCommands (const $ Just 1.23) now >-> emitJsonStdout

bar path now =  evalHState $ skipHeader path >-> toHoogleLine >-> toFunctionInfo
                                  >-> toCommands (const $ Just 1.23) now >-> emitJsonStdout


test4 path = do
  count <- P.fold (\x _ -> x+1) 0 id (textLines path)
  putStrLn $ path ++ ": " ++ show count
-}

