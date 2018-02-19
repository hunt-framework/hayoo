module Hayoo.CLI.Server
  ( Command
  , parser
  , run
  ) where


import           Data.Monoid         ((<>))
import qualified Data.Text           as T
import qualified Hayoo.Server        as Server
import           Options.Applicative
import qualified Servant.Client      as SC



-- COMMAND


data Command
  = Start Server.Config


-- RUN


run :: Command -> IO ()
run cmd =
  case cmd of
    Start config ->
      Server.runServer config



-- PARSER


parser :: Parser Command
parser =
  let
    config =
      Server.Config <$> serverConfig <*> huntConfig
  in
    Start <$> config


huntConfig :: Parser Server.HuntConfig
huntConfig =
  let
    baseUrlReader :: ReadM SC.BaseUrl
    baseUrlReader =
      eitherReader $ \s ->
        case SC.parseBaseUrl s of
          Left ex ->
            Left (show ex)

          Right bu ->
            pure bu

    baseUrl =
      option baseUrlReader
        ( long "hunt-url"
        <> short 'b'
        <> help "Base URL for Hunt server"
        )
  in
    Server.HuntConfig <$> baseUrl


serverConfig :: Parser Server.ServerConfig
serverConfig =
  let
    host =
      T.pack <$> strOption
        ( long "host"
        <> short 'h'
        <> help "Host of the server"
        )

    port =
      option auto
        ( long "port"
        <> short 'p'
        <> help "Port of the server, default 3001"
        )

    publicDir =
      strOption
        ( long "public-directory"
        <> short 'd'
        <> help "Public directory of static files"
        )
  in
    Server.ServerConfig <$> host <*> port <*> publicDir
