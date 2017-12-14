module Hayoo.Indexer
  ( Config (..)
  , Mode (..)
  , Statistics (..)
  , run
  ) where


import qualified Control.Monad                 as Monad
import qualified Data.Aeson                    as Json
import qualified Data.Aeson.Encode.Pretty      as Json
import qualified Data.ByteString.Lazy.Char8    as LBS
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Time                     as Time
import qualified Data.Vector                   as Vector
import qualified Hayoo.Indexer.DeclInfo.Hoogle as Hoogle
import qualified Hayoo.Indexer.DeclInfo.Index  as DeclInfo
import qualified Hayoo.Indexer.Schema          as Schema
import qualified System.Directory              as Directory
import qualified Text.Megaparsec               as M



-- INDEXING


-- |
data Config
  = Config
    { _outputDirectory :: FilePath
    , _mode            :: Mode
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
    { _hoogleFile :: FilePath
    , _withSchema :: Bool
    }
  | CabalFile
    { _cabalFile  :: FilePath
    , _withSchema :: Bool
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
run :: Config -> IO Statistics
run config =
  case _mode config of
    HoogleFile filePath schema -> do
      hoogleFile filePath schema (_outputDirectory config)
      pure Statistics

    _ -> do
      putStrLn "Sorry, this is not supported yet."
      pure Statistics


hoogleFile :: FilePath -> Bool -> FilePath -> IO ()
hoogleFile hoogleFilePath schema outputDirectory = do
  content <- T.readFile hoogleFilePath
  let result = Hoogle.parse content
  case result of
    Left err -> do
      putStrLn (M.parseErrorPretty' content err)

    Right (pkg, infos) -> do
      Directory.createDirectoryIfMissing True outputDirectory

      Monad.when schema $ do
        LBS.writeFile "schema.json" (Json.encodePretty Schema.insert)

      now <- Time.getCurrentTime
      let jsonInfos = map (DeclInfo.insert now) infos
      LBS.writeFile
        (T.unpack (Hoogle._name pkg) ++ ".json")
        (Json.encodePretty (Json.Array (Vector.fromList jsonInfos)))
