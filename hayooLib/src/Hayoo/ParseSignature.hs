{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- {-# LANGUAGE FlexibleInstances, OverlappingInstances, IncoherentInstances #-}
-- {-# LANGUAGE OverloadedStrings #-}

module Hayoo.ParseSignature
    ( SignatureT (..)
    , Signature
    , parseSignature
    , prettySignature
    , normalizeSignature
    , expand
    , expandNormalized
    , modifySignatureWith

    , subSignatures
    , normSignatures
    , normSignature
    )
where

import           Prelude                hiding (mapM, sequence)

import           Data.Char
import           Data.Foldable          (Foldable)
import           Data.List
import           Data.String            (IsString, fromString)
import           Data.Traversable       (Traversable, mapM)

import           Control.Applicative    ((*>), (<$>), (<*))
import           Control.Monad.Identity (Identity)
import           Control.Monad.State    (State, get, put, runState)

import           Text.Parsec            (ParseError, ParsecT, alphaNum, char,
                                         eof, many1, parse, sepBy, spaces,
                                         string, try, (<?>), (<|>))


data SignatureT a =
      Symbol   { getSymbol :: a }
    | TypeApp  { getPrefix    :: a , getPostfix :: [SignatureT a] }
    | Tuple    { getElements :: [SignatureT a] }
    | Function { getParameter :: SignatureT a, getResult :: SignatureT a }
    | Context  { getContext   :: SignatureT a, getType   :: SignatureT a }
    | Equiv    { getLeft      :: SignatureT a, getRight  :: SignatureT a }
    deriving (Eq, Functor, Foldable, Traversable, Show)

type Signature = SignatureT String

type StringParsec r = ParsecT String () Identity r

symbol :: StringParsec String
symbol
    = (many1 $ (alphaNum <|> char '.' <|> char '\'' <|> char '_')) <* spaces

symbol' :: StringParsec Signature
symbol'
    = Symbol <$> symbol
      <?> "symbol"

typeBranch :: StringParsec Signature
typeBranch
    = tuple
      <|> list
      <|> try symbol'

typeApplication :: StringParsec Signature
typeApplication = do
  prefix <- symbol
  ((TypeApp prefix) <$> (many1 typeBranch))
    <|> return (Symbol prefix)

exprBranch :: StringParsec Signature
exprBranch
    = tuple
      <|> list
      <|> typeApplication
      <?> "expression"

expr :: StringParsec Signature
expr = do
    btype <- exprBranch
    (     Function btype <$> (string "->" *> spaces *> expr))
      <|> (Context btype <$> (string "=>" *> spaces *> expr))
      <|> (Equiv   btype <$> (string "~"  *> spaces *> expr))
      <|> return btype

list :: StringParsec Signature
list = (TypeApp "[]" . return) <$> brackets expr

tuple :: StringParsec Signature
tuple = do
    elems <- (parens $ sepBy expr $ (char ',' *> spaces))
    case elems of
        []  -> return $ Symbol "()"
        [e] -> return e
        _   -> return $ Tuple elems

brackets ::  StringParsec Signature -> StringParsec Signature
brackets sub
    = (char '[' *> spaces) *> sub <* (char ']' *> spaces)

parens ::  StringParsec a -> StringParsec a
parens sub
    = (char '(' *> spaces) *> sub <* (char ')' *> spaces)

withEof :: StringParsec Signature -> StringParsec Signature
withEof content
    = content <* eof

parseUnknown :: StringParsec Signature -> String -> Either ParseError Signature
parseUnknown e
    = parse (withEof e) "(unknown)"

parseSignature :: String -> Either ParseError Signature
parseSignature
    = parseUnknown expr

instance IsString Signature where
    fromString s = either (error . show) id $ parseSignature s

-- ----------------------

addParens :: Signature -> String
addParens s                    = "(" ++ (pretty s) ++ ")"

checkParens :: Signature -> String
checkParens s@Symbol{}         = pretty s
checkParens s@Tuple{}          = pretty s
checkParens s@(TypeApp "[]" _) = pretty s
checkParens s                  = addParens s

checkParensf :: Signature -> String
checkParensf s@TypeApp{}       = pretty s
checkParensf s                 = checkParens s

pretty :: Signature -> String
pretty (Symbol i)              = i
pretty (TypeApp "[]" c)        = "[" ++ (pretty $ head c) ++ "]"
pretty (TypeApp p c)           = p ++ " " ++ (intercalate " " $ map checkParens c)
pretty (Tuple c)               = "(" ++ (intercalate "," $ map pretty c) ++ ")"
pretty (Function p r)          = (checkParensf p) ++ "->" ++ (pretty r)
pretty (Context  c t)          = (checkParensf c) ++ "=>" ++ (pretty t)
pretty (Equiv    l r)          = (pretty       l) ++ "~"  ++ (pretty r)

prettySignature :: Signature -> String
prettySignature                = pretty

-- ----------------------

children :: Signature -> [Signature]
children = nub . children'
    where
    children' :: Signature -> [Signature]
    children' Symbol{}                 = []
    children' TypeApp {getPostfix = p} = p
    children' (Tuple e)                = e ++ (concatMap children' e)
    children' (Function param result)  = param : result : children' param ++ children' result
    children' (Context  cx    ty    )  =         ty     : cxElems cx      ++ children' ty
    children' (Equiv    l     r     )  = l     : r      : children' l     ++ children' r
    -- @childen'@ called instead of @childen@, else @nub@ is called more than once

cxElems :: Signature -> [Signature]
cxElems (Tuple cxs) = cxs
cxElems cx          = [cx]

nextKey :: [(String, String)] -> String
nextKey []           = "a"
nextKey ((_, k) : _) = nextKey' k
    where
    nextKey' :: String -> String
    nextKey' k'
        = head
          $ tail
          $ dropWhile (/= k')
          $ (map return ['a'..'z']) ++ [x:y:[] | x <- ['a'..'z'], y <- ['a'..'z']]

normalizeSignature :: Signature -> (Signature, [(String, String)])
normalizeSignature sig
    = runState (mapM norm sig) []
    where
      norm :: String -> State [(String, String)] String
      norm sym
          | isUpper $ head sym = return sym
          | sym == "[]"        = return sym
          | otherwise          = do st <- get
                                    case lookup sym st of
                                      Just s  -> return s
                                      Nothing -> let s = nextKey st in
                                                 do put $ (sym, s) : st
                                                    return s

parents :: Signature -> Signature
parents s@Symbol{}                   = s
parents (TypeApp _  (s@Symbol{}:[])) = s
parents (TypeApp pre post)           = TypeApp pre $ parents <$> post
parents (Tuple e)                    = Tuple $ map parents e
parents (Function param result)      = Function (parents param) (parents result)
parents (Context  cx    ty    )      = Context  cx              (parents ty    )
parents e@Equiv{}                    = e

countComplex :: Signature -> Int
countComplex Symbol{}                = 0
countComplex (TypeApp _ post)        = length post  + (sum $ countComplex <$> post)
countComplex (Tuple e)               = length e - 1 + (sum $ map countComplex e)
countComplex (Function param result) = 1 + countComplex param + countComplex result
countComplex (Context  cx    ty    ) = 1 + countComplex cx    + countComplex ty
countComplex (Equiv    l     r     ) = 1 + countComplex l     + countComplex r

isCompex :: Signature -> Bool
isCompex c = countComplex c >= 3

expand :: Signature -> [Signature]
expand s = nub $ s : (filter isCompex $ expand' s)

expand' :: Signature -> [Signature]
expand' s = parents s : (parents $ parents s): children s

expandNormalized :: Signature -> [Signature]
expandNormalized = map (fst . normalizeSignature) . expand

modifySignatureWith :: (Signature -> [Signature]) -> String -> String
modifySignatureWith func sig
    = either (const sig) (intercalate "$") $ pretty <$$> (func <$> parseSignature sig)

(<$$>) :: (Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)
(<$$>) f x = (fmap.fmap) f x

subSignatures :: Signature -> [Signature]
subSignatures
    = nub . subs
    where
      subs (Context cx ty) = cxElems cx ++ subs ty
      subs  Equiv{}        = []
      subs  s              = filter isCompex $ expand' s

normSignatures :: [Signature] -> [Signature]
normSignatures
    = nub . map normSignature

normSignature :: Signature -> Signature
normSignature = fst . normalizeSignature
