module Hayoo.CLI.Indexer
  ( Command
  , parser
  , run
  ) where


import qualified Control.Monad       as Monad
import           Data.Monoid         ((<>))
import qualified Hayoo.Indexer       as Indexer
import           Options.Applicative



-- COMMAND


data Command
  = Index Indexer.Config



-- RUN


run :: Command -> IO ()
run cmd =
  case cmd of
    Index config ->
      Monad.void (Indexer.run config)



-- PARSER


parser :: Parser Command
parser = Index <$> subparser
  ( command "manual" ( info manualOpts ( progDesc "Indexing the whole of Hoogle manually by providing the files yourself" ))
  <> command "hoogle-file" ( info hoogleFileOpts ( progDesc "Indexing a single hoogle file" ))
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
