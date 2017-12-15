{-# LANGUAGE RankNTypes #-}
module Hayoo.Indexer
  ( Config (..)
  , Mode (..)
  , Statistics (..)
  , run
  ) where


import           Control.Applicative           ((<|>))
import qualified Control.Monad                 as Monad
import qualified Control.Monad.IO.Class        as Monad
import qualified Control.Monad.Trans.Resource  as Resource
import qualified Data.Aeson                    as Json
import qualified Data.Aeson.Encode.Pretty      as Json
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as LBS
import           Data.Conduit                  ((.|))
import qualified Data.Conduit                  as Conduit
import qualified Data.Conduit.Combinators      as Conduit
import qualified Data.Conduit.Tar              as Conduit
import qualified Data.Conduit.Zlib             as Conduit
import           Data.Semigroup                ((<>))
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Encoding.Error      as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy.Encoding       as LT
import qualified Data.Time                     as Time
import qualified Data.Vector                   as Vector
import qualified Data.Void                     as Void
import qualified Hayoo.Indexer.DeclInfo.Hoogle as Hoogle
import qualified Hayoo.Indexer.DeclInfo.Index  as DeclInfo
import qualified Hayoo.Indexer.Schema          as Schema
import qualified System.Directory              as Directory
import           System.FilePath
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
    { indexedFiles :: Int
    , erroredFiles :: Int
    } deriving (Show)


-- | @index@ hoogle and cabal files for @pump@ing them into
-- the hunt searchengine.
run :: Config -> IO Statistics
run config =
  case _mode config of
    HoogleFile filePath schema -> do
      Directory.createDirectoryIfMissing True (_outputDirectory config)
      Monad.when schema (schemaFile (_outputDirectory config))
      hoogleFile filePath (_outputDirectory config)
      pure (Statistics 1 0)

    Manual hoogleArchive cabalArchive -> do
      manual hoogleArchive cabalArchive (_outputDirectory config)
      pure (Statistics 0 0)

    _ -> do
      putStrLn "Sorry, this is not supported yet."
      pure (Statistics 0 0)


hoogleFile :: FilePath -> FilePath -> IO ()
hoogleFile hoogleFilePath outputDirectory = do
  content <- T.readFile hoogleFilePath
  let result = Hoogle.parse content
  case result of
    Left err -> do
      putStrLn (M.parseErrorPretty' content err)

    Right (pkg, infos) -> do
      now <- Time.getCurrentTime
      let jsonInfos = map (DeclInfo.insert now) infos
      LBS.writeFile
        (outputDirectory </> (T.unpack (Hoogle._name pkg) ++ ".json"))
        (Json.encodePretty (Json.Array (Vector.fromList jsonInfos)))


schemaFile :: FilePath -> IO ()
schemaFile output =
  LBS.writeFile
    (output </> "schema.json")
    (Json.encodePretty Schema.insert)


-- MANUAL


type HoogleParseResult
  = Either (FilePath, String) (Hoogle.Package, Json.Value)


manual :: FilePath -> FilePath -> FilePath -> IO ()
manual hoogleTar _cabalTar outputDir = do
  schemaFile outputDir
  Conduit.runConduitRes $
    Conduit.sourceFileBS hoogleTar
      .| Conduit.ungzip
      .| Conduit.untar
      .| Conduit.withEntries (handleFile outputDir)
      .| Conduit.print


handleFile :: Monad.MonadIO m
           => FilePath
           -> Conduit.Header
           -> Conduit.Conduit BS.ByteString m Statistics
handleFile outputDir header =
  decodeUtf8
    -- For some time now, all documentation on hackage should
    -- be utf8 encoded. However, there are packages like FieldTrip,
    -- for which this is not true. Since they are very old, we
    -- can safely ignore them here.
    .| ignoreError
    .| keepFiles header
    .| parseHoogle
    .| handleParseResult outputDir


parseHoogle :: Monad.MonadIO m => Conduit.Conduit (FilePath, T.Text) m HoogleParseResult
parseHoogle = do
  Conduit.awaitForever $ \(path, content) ->
    case Hoogle.parse content of
      Left err ->
        err
          |> M.parseErrorPretty' content
          |> (\c -> Left (path, c))
          |> Conduit.yield

      Right (pkg, infos) -> do
        now <- Monad.liftIO Time.getCurrentTime
        infos
          |> map (DeclInfo.insert now)
          |> Vector.fromList
          |> Json.Array
          |> (\j -> (pkg, j))
          |> Right
          |> Conduit.yield


handleParseResult :: Monad.MonadIO m => FilePath -> Conduit.Conduit HoogleParseResult m Statistics
handleParseResult output =
  Conduit.awaitForever $ \result ->
    case result of
      Left (filePath, parseError) -> do
        Monad.liftIO (putStrLn (filePath <> ":\n" <> parseError))
        pure ()

      Right (pkg, json) ->
        let
          filePath =
            output </> T.unpack (Hoogle._name pkg) <> ".json"
        in do
          Monad.liftIO (putStrLn ("Writing: " <> filePath))
          Monad.liftIO (LBS.writeFile filePath (Json.encodePretty json))
          pure ()



-- HELPERS


keepFiles :: Monad m => Conduit.Header -> Conduit.Conduit o m (FilePath, o)
keepFiles header =
  case Conduit.headerFileType header of
    Conduit.FTNormal ->
      Conduit.awaitForever $ \content ->
        Conduit.yield (Conduit.headerFilePath header, content)

    _ ->
      pure ()


decodeUtf8 :: Monad m => Conduit.Conduit BS.ByteString m (Either T.UnicodeException T.Text)
decodeUtf8 =
  Conduit.awaitForever $ \content ->
    Conduit.yield (T.decodeUtf8' content)


ignoreError :: Monad m => Conduit.Conduit (Either e a) m a
ignoreError =
  Conduit.awaitForever $ \content ->
    case content of
      Left _err ->
        pure ()

      Right result ->
        Conduit.yield result


(|>) :: a -> (a -> b) -> b
(|>) a f =
  f a
