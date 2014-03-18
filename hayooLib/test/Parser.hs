{-# LANGUAGE OverloadedStrings #-}

module Main where
{-- Tests for Normalizers Analyzers Formatters #-}


import           Test.Framework
import           Test.Framework.Providers.HUnit
-- import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit
-- import           Test.QuickCheck
--import qualified Test.QuickCheck.Monadic                as QM

import           Hayoo.ParseSignature
-- ----------------------------------------------------------------------------

main :: IO ()
main = defaultMain
    ([
        --testCase "p1"             test_parse_1
    --, testProperty "Normalizer: date YYYYMMDD"            prop_isAnyDate
    ] ++ simpleTests)

-- ----------------------------------------------------------------------------

--prop_isInt_int :: Gen Bool
--prop_isInt_int = do
--    val <- arbitrary :: Gen Int
--    return . NI.isInt . T.pack . show $ val

parsers :: [(Signature, String)]
parsers = 
    [
         (Symbol "a",                                       "a")
       , (TypeApp "a" [Symbol "b"],                         "a b")
       , (TypeApp "[]" [Symbol "a"],                        "[a]")
       , ((Symbol "a") `Function` (Symbol "b"),             "a -> b")
       , (Symbol "()",                                      "()")
       , (Symbol "a",                                       "(a)")
       , (Tuple [Symbol "a", Symbol "b"],                   "(a,b)")
       , (Tuple [Symbol "a", Symbol "b"],                   "( a , b )")
       , ((Symbol "a") `Function` (TypeApp "b" [Symbol "c"]), "a -> b c")
       , ((TypeApp "a" [Symbol "b"]) `Function` (Symbol "c"), "a b -> c")
       , ( (Symbol "a") `Function` ((Symbol "b")  `Function` (Symbol "c")), "a -> b -> c")
       , (((Symbol "a") `Function`  (Symbol "b")) `Function` (Symbol "c"), "(a -> b) -> c")
       , (TypeApp "a" [(Symbol "b"), (Symbol "c")],         "a b c")
       , (TypeApp "a" [(Symbol "b"), (Symbol "c")] `Function` (Symbol "d"),         "a b c -> d")
       , (TypeApp "[]" [TypeApp "a" [(Symbol "b"), (Symbol "c")]],         "[a b c]")
    ]

simpleTests :: [Test.Framework.Test]
simpleTests = map toTest $ parsers
    where
        toTest (sig, text) = testCase ("Test: " ++ text) $ sig @=? eqParse text

eqParse :: String -> Signature
eqParse s = case parseSignature s of 
    (Right sig) -> sig
    (Left err) -> Symbol $ show err