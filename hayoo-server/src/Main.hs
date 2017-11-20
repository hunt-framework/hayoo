{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Data.Monoid         ((<>))
import qualified Hayoo.Server        as Hayoo
import           Options.Applicative



-- MAIN


main :: IO ()
main =
  let
    config = Just <$> strOption
      ( long "config"
      <> short 'c'
      <> metavar "CONFIG_FILE"
      <> help "Path to the .toml configuration file"
      )

    opts = info (helper <*> (config <|> pure Nothing))
      ( fullDesc
      <> progDesc "Start the Hayoo! server"
      <> header "A command line interface to configure the startup of the Hayoo! server" )
  in do
    maybeConfigFilePath <- execParser opts
    case maybeConfigFilePath of
      Nothing ->
        Hayoo.runServer Hayoo.defaultConfig

      Just configFilePath -> do
        config <- Hayoo.readTomlFile configFilePath
        either putStrLn Hayoo.runServer config
