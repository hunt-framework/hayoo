module Main where


import qualified Control.Monad       as Monad
import           Data.Monoid         ((<>))
import qualified Hayoo.Indexer       as Indexer
import           Options.Applicative



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
      Monad.void $
          Indexer.run $
            Indexer.Config outputDir (Indexer.HoogleFile filePath withSchema)

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
