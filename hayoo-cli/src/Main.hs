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
  = Index Indexer.Config
  | Server



-- RUN COMMAND


run :: Command -> IO ()
run c =
  case c of
    Index config -> do
      Monad.void (Indexer.run config)

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
indexOpts = Index <$> subparser
  ( command "manual" ( info manualOpts ( progDesc "Indexing the whole of Hoogle manually." ) )
  <> command "hoogle-file" ( info hoogleFileOpts ( progDesc "Indexing a single hoogle file." ) )
  )


outputDir :: Parser FilePath
outputDir =
  strOption
    ( long "output-directory"
    <> short 'o'
    <> help "Directory with the resulting files"
    )


manualOpts :: Parser Indexer.Config
manualOpts =
  Indexer.Config <$> outputDir
                 <*> (Indexer.Manual <$> hoogleArchive <*> cabalArchive)
  where
    hoogleArchive =
      strOption
        ( long "hoogle-archive"
        <> short 'h'
        <> help "Path to the hoogle tar archive to be indexed"
        )

    cabalArchive =
      strOption
        ( long "cabal-archive"
        <> short 'c'
        <> help "Path to the hoogle tar archive to be indexed"
        )


hoogleFileOpts :: Parser Indexer.Config
hoogleFileOpts =
  Indexer.Config <$> outputDir
                 <*> (Indexer.HoogleFile <$> filePath <*> withSchema)
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




serverOpts :: Parser Command
serverOpts =
  pure Server
