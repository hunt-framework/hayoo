module Main where


import qualified Hayoo.Indexer.Hoogle.ParseTest as Hoogle
import           Test.Hspec                     (describe, hspec)


main :: IO ()
main =
  hspec $ do
    describe "Hoogle" Hoogle.suite
