module Main where

import qualified MainCabal as MC
import qualified MainHoogle as MH
import Paths_hayooIndexer
import System.Directory (copyFile, createDirectoryIfMissing)
import System.IO
import Options.Applicative

data Opts = Opts { cabalArchive :: Maybe String
                 , hoogleArchive :: Maybe String
                 }

opts :: Parser Opts
opts = Opts
  <$> optional (strOption
      ( long "cabal"
     <> short 'c'
     <> metavar "FILE"
     <> help "cabal tar archive file, e.g. index.tar.gz"
      ))
  <*> optional (strOption
      ( long "hoogle"
     <> short 'h'
     <> metavar "FILE"
     <> help "hoogle archive file, e.g. hoogle.tar.gz"
      ))

main = execParser inf >>= main'
  where inf = info (helper <*> opts)
                   (fullDesc 
                 <> progDesc "Generate index commands for Hayoo"
                 <> header "hayooIndexer - Generate index commands for Hayoo" )

main' :: Opts -> IO ()
main' options = do
  hSetBuffering stdout NoBuffering

  -- create json/00-schema.js
  schemaPath <- getDataFileName "00-schema.js"
  createDirectoryIfMissing True "json"
  copyFile schemaPath "json/00-schema.json"
  putStrLn "copied 00-schema.json"

  -- create 01-packages.js and 02-ranking.js
  case cabalArchive options of
    Nothing -> return ()
    Just x  -> MC.main x

  -- create the function info commands
  case hoogleArchive options of
    Nothing -> return ()
    Just x  -> MH.main x

