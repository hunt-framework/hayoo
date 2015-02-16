module Hayoo.Haskell where

import           Language.Haskell.Exts.Extension
import qualified Language.Haskell.Exts.Parser as Parser
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.Comments

pphsMode :: PPHsMode
pphsMode = PPHsMode {
    classIndent   = 0
  , doIndent      = 0
  , multiIfIndent = 0
  , caseIndent    = 0
  , letIndent     = 0
  , whereIndent   = 0
  , onsideIndent  = 0
  , spacing       = False
  , layout        = PPNoLayout
  , linePragmas   = False
  }

pretty :: Pretty a => a -> String
pretty = prettyPrintWithMode pphsMode

parseMode :: Parser.ParseMode
parseMode =
  Parser.defaultParseMode {
    Parser.extensions = parseModeExtensions
    }

parseModeExtensions :: [Extension]
parseModeExtensions =
  fmap EnableExtension [
      ConstraintKinds
    , EmptyDataDecls
    , TypeOperators
    , ExplicitForAll
    , GADTs
    , KindSignatures
    , MultiParamTypeClasses
    , TypeFamilies
    , FlexibleContexts
    , FunctionalDependencies
    , ImplicitParams
    , MagicHash
    , UnboxedTuples
    , ParallelArrays
    , UnicodeSyntax
    , DataKinds
    , PolyKinds
    ]

parse :: Parser.Parseable a => String -> Either String a
parse s =
  case Parser.parseWithMode parseMode s of
   Parser.ParseOk a         -> return a
   Parser.ParseFailed _ err -> Left err

parseWithComments :: Parser.Parseable a => String -> Either String (a, [Comment])
parseWithComments s =
  case Parser.parseWithComments parseMode s of
   Parser.ParseOk a -> return a
   Parser.ParseFailed srcLoc err -> Left (err ++ " (" ++ show srcLoc ++ ")")
