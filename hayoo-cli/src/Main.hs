module Main where


import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import qualified Hayoo.Indexer.Hoogle.Parse as Hoogle
import           Options.Applicative
import qualified Text.Megaparsec.Error      as M



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
    { file :: FilePath
    } deriving (Show)



-- RUN COMMAND


run :: Command -> IO ()
run c =
  case c of
    Index (HoogleFile filePath) -> do
      content <- T.readFile filePath
      let x = Hoogle.parse content
      case x of
        Right result ->
          putStrLn (show result)

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
  Index <$> (HoogleFile <$> filePath)
  where
    filePath =
      strOption
        ( long "file"
        <> short 'f'
        <> help "Path to the hoogle file to be indexed"
        )



serverOpts :: Parser Command
serverOpts =
  pure Server
