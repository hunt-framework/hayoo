module Hayoo.CLI
  ( Command
  , parser
  , run
  ) where


import           Data.Monoid         ((<>))
import qualified Hayoo.CLI.Indexer   as Indexer
import qualified Hayoo.CLI.Server    as Server
import           Options.Applicative



-- COMMAND


data Command
  = Indexer Indexer.Command
  | Server Server.Command



-- PARSER


parser :: ParserInfo Command
parser =
  info (combined <**> helper)
    ( fullDesc
    <> progDesc "Run one of the available Hayoo! commands"
    <> header "hayoo - A command line interface to interact with Hayoo!"
    )


combined :: Parser Command
combined =
  subparser
    ( command "indexer" (Indexer <$> info (Indexer.parser <**> helper) (progDesc "Interact with the indexer"))
    <> command "server" (Server <$> info (Server.parser <**> helper) (progDesc "Start and control the Hayoo! server"))
    )



-- RUN


run :: Command -> IO ()
run cmd =
  case cmd of
    Indexer subCmd ->
      Indexer.run subCmd

    Server subCmd ->
      Server.run subCmd
