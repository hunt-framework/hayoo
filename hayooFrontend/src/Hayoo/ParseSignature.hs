{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-} 
-- {-# LANGUAGE FlexibleInstances, OverlappingInstances, IncoherentInstances #-} 
-- {-# LANGUAGE OverloadedStrings #-}

module Hayoo.ParseSignature 
(
  SignatureT (..)
, Signature
, parseSignature
, prettySignature
, normalizeSignature
, expand
, expandNormalized
) where

import Prelude hiding (mapM, sequence)

import Data.Char
import Data.Foldable (Foldable)
import Data.List
import Data.String (IsString, fromString)
import Data.Traversable (Traversable, mapM)

import Control.Applicative    ((*>), (<*), (<$>)) -- , (<*>)
import Control.Monad.Identity (Identity)
import Control.Monad.State (MonadState, State, runState, get, put)

import Text.Parsec (many1, parse, spaces, (<|>), (<?>), char, alphaNum, string, sepBy, ParsecT, eof, ParseError, try)


data SignatureT a = 
      Symbol { getSymbol :: a }
    | TypeApp { getPrefix :: a , getPostfix :: [SignatureT a] }
    | Tuple { getElements :: [SignatureT a] }
    | Function { getParameter :: SignatureT a, getResult :: SignatureT a }
    deriving (Eq, Functor, Foldable, Traversable, Show)

type Signature = SignatureT String

type StringParsec r = ParsecT String () Identity r

symbol :: StringParsec String
symbol = (many1 $ (alphaNum <|> char '.')) <* spaces

symbol' :: StringParsec Signature
symbol' = Symbol <$> symbol <?> "symbol"

typeBranch :: StringParsec Signature
typeBranch = tuple <|> list <|> (try symbol')

typeApplication :: StringParsec Signature
typeApplication = do
    prefix <- symbol
    ((TypeApp prefix) <$> (many1 typeBranch)) <|> return (Symbol prefix)

exprBranch :: StringParsec Signature
exprBranch = tuple <|> list <|> typeApplication <?> "expression"

expr :: StringParsec Signature
expr = do
    btype <- exprBranch
    (Function btype <$> (string "->" *> spaces *> expr) ) <|> return btype

list :: StringParsec Signature
list = (TypeApp "[]" . return) <$> braces expr

tuple :: StringParsec Signature
tuple = do
    elems <- (parens $ sepBy expr $ (char ',' *> spaces))
    case elems of
        [] -> return $ Symbol "()"
        [e] -> return e
        _ -> return $ Tuple elems

braces ::  StringParsec Signature -> StringParsec Signature
braces sub = (char '[' *> spaces) *> sub <* (char ']' *> spaces)

parens ::  StringParsec a -> StringParsec a
parens sub = (char '(' *> spaces) *> sub <* (char ')' *> spaces)

withEof :: StringParsec Signature -> StringParsec Signature
withEof content = content <* eof

parseUnknown :: StringParsec Signature -> String -> Either ParseError Signature
parseUnknown e = parse (withEof e) "(unknown)"

parseSignature :: [Char] -> Either ParseError Signature
parseSignature = parseUnknown expr


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
checkParensf s = checkParens s 

pretty :: Signature -> String
pretty (Symbol i)              = i
pretty (TypeApp "[]" c)        = "[" ++ (pretty $ head c) ++ "]"
pretty (TypeApp p c)           = p ++ " " ++ (intercalate " " $ map checkParens c)
pretty (Tuple c)               = "(" ++ (intercalate "," $ map pretty c) ++ ")"
pretty (Function p r)          = (checkParensf p) ++ "->" ++ (pretty r)

prettySignature :: Signature -> String
prettySignature = pretty

-- ----------------------

children :: Signature -> [Signature]
children = nub . children'
    where
    children' :: Signature -> [Signature]
    children' Symbol{} = []
    children' TypeApp {getPostfix = p} = p
    children' (Tuple e) = e ++ (concatMap children e)
    children' (Function param result) = param : result : children result


nextKey :: [(String, String)] -> String
nextKey [] = "a"
nextKey ((_,k):_) = nextKey' k
    where
    nextKey' :: String -> String
    nextKey' k' = head $ tail $ dropWhile (/= k') $ (map return ['a'..'z']) ++ [x:y:[] | x <- ['a'..'z'], y <- ['a'..'z']]

normalizeSignature :: Signature -> (Signature, [(String, String)])
normalizeSignature sig = runState (mapM norm sig) []
    where
    norm :: String -> State [(String, String)] String
    norm sym 
        | isUpper $ head sym = return sym
        | sym == "[]" = return sym
        | otherwise = do
            st <- get
            case lookup sym st of
                (Just s) -> return s
                Nothing -> do
                    let s = nextKey st
                    put $ (sym, s):st
                    return s


parents :: Signature -> Signature
parents s@Symbol{}                   = s
parents (TypeApp _  (s@Symbol{}:[])) = s
parents (TypeApp pre post)           = TypeApp pre $ parents <$> post
parents (Tuple e)                    = Tuple $ map parents e
parents (Function param result)      = Function (parents param) (parents result)


countComplex :: Signature -> Int
countComplex Symbol{}                = 0
countComplex (TypeApp _ post)        = 1 + (sum $ countComplex <$> post)
countComplex (Tuple e)               = 1 + (sum $ map countComplex e)
countComplex (Function param result) = 1 + countComplex param + countComplex result

isCompex :: Signature -> Bool
isCompex f@Function{}                = countComplex f >= 3
isCompex _                           = False

expand :: Signature -> [Signature]
expand s = s : (filter isCompex $ parents s : (parents $ parents s): children s)

expandNormalized :: Signature -> [Signature]
expandNormalized = map (fst . normalizeSignature) . expand

--(<$$>) :: (Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)
--(<$$>) f x = (fmap.fmap) f x