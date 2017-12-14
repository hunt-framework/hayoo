module Main where


import qualified Control.Monad                 as Monad
import qualified Data.Aeson                    as Json
import qualified Data.Aeson.Encode.Pretty      as Json
import qualified Data.ByteString.Lazy.Char8    as BS
import           Data.Monoid                   ((<>))
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Time                     as Time
import qualified Data.Vector                   as Vector
import qualified Hayoo.Core.DeclInfo           as DeclInfo
import qualified Hayoo.Indexer.DeclInfo.Hoogle as Hoogle
import qualified Hayoo.Indexer.DeclInfo.Index  as DeclInfo
import qualified Hayoo.Indexer.Schema          as Schema
import           Options.Applicative
import qualified System.Directory              as Directory
import qualified Text.Megaparsec.Error         as M



-- MAIN


main :: IO ()
main = execParser opts >>= run
  where
    opts =
      info (cmd <**> helper)
        ( fullDesc
        <> progDesc "Go"
        <> header "hayoo-cli - A simple interface for interacting with Hayoo!"
        )



-- COMMAND


data Command
  = Index IndexCommand
  | Server


data IndexCommand
  = HoogleFile
    { _file       :: FilePath
    , _withSchema :: Bool
    , _ouputDir   :: FilePath
    } deriving (Show)



-- RUN COMMAND


run :: Command -> IO ()
run c =
  case c of
    Index (HoogleFile filePath withSchema outputDir) -> do
      content <- T.readFile filePath
      let result = Hoogle.parse content
      case result of
        Right (pkg, infos) -> do
          Directory.createDirectoryIfMissing True outputDir

          Monad.when withSchema $ do
            BS.writeFile "schema.json" (Json.encodePretty Schema.insert)

          now <- Time.getCurrentTime
          let jsonInfos = map (DeclInfo.insert now) infos
          BS.writeFile
            (T.unpack (Hoogle._name pkg) ++ ".json")
            (Json.encodePretty (Json.Array (Vector.fromList jsonInfos)))

        Left err ->
          putStrLn (M.parseErrorPretty' content err)

    Server ->
      putStrLn "this is not implemented yet"



-- PARSE COMMAND OPTIONS


cmd :: Parser Command
cmd =
  subparser
  ( command "index" ( info indexOpts ( progDesc "Indexing Hackage for Hayoo!" ) )
  <> command "server" ( info serverOpts ( progDesc "Start and control the Hayoo! server" ) )
  )


indexOpts :: Parser Command
indexOpts =
  Index <$> (HoogleFile <$> filePath <*> withSchema <*> outputDir)
  where
    filePath =
      strOption
        ( long "file"
        <> short 'f'
        <> help "Path to the hoogle file to be indexed"
        )

    withSchema =
      switch
        ( long "with-schema"
        <> help "Should the schema be printed as well?"
        ) <|> pure False

    outputDir =
      strOption
        ( long "output"
        <> short 'o'
        <> help "Directory with the resulting files"
        )


serverOpts :: Parser Command
serverOpts =
  pure Server
