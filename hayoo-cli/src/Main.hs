module Main where


import qualified Hayoo.CLI           as CLI
import qualified Options.Applicative as Options



-- MAIN


main :: IO ()
main =
  Options.execParser CLI.parser >>= CLI.run
