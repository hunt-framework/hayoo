{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances, IncoherentInstances #-} 
{-# LANGUAGE OverloadedStrings #-}

module Hayoo.ParseSignature where

import Prelude hiding (mapM, sequence)

import Data.Char
import Data.String (IsString, fromString)
import Text.Parsec (many1, parse, spaces, (<|>), char, alphaNum, string, sepBy, ParsecT, eof, ParseError)
import Data.List

import Control.Monad.Identity (Identity)
import Control.Monad.State (MonadState, State, runState, get, put)
import Control.Applicative    ((*>), (<*), (<$>))
-- import Data.Functor (Functor)
import Data.Foldable (Foldable, fold)
import Data.Traversable (Traversable, mapM, traverse, sequence)

data SignatureT a = 
      Symbol { getSymbol :: a }
    | TypeApp { getPrefix :: a , getPostfix :: SignatureT a }
    | Tuple { getElements :: [SignatureT a] }
    | Function { getParameter :: SignatureT a, getResult :: SignatureT a }
    deriving (Eq, Show, Functor, Foldable, Traversable)

type Signature = SignatureT String

instance Show Signature where
    show (Symbol i) = i
    show (TypeApp "[]" c)          = "[" ++ (show c) ++ "]"
    show (TypeApp p c@(Symbol {})) = p ++ " " ++ (show c)
    show (TypeApp p c@(Tuple {}))  = p ++ " " ++ (show c)
    show (TypeApp p c)             = p ++ " (" ++ (show c) ++ ")"
    show (Tuple c)                 = "(" ++ (intercalate "," $ map show c) ++ ")"
    show (Function p@Function{} r) = "(" ++ (show p) ++ ")->" ++ (show r)
    show (Function p r)            = (show p) ++ "->" ++ (show r)


type StringParsec u r =  ParsecT String u Identity r

symbol :: StringParsec u String
symbol = (many1 $ (alphaNum <|> char '.')) <* spaces

-- spaces = many space

typeBranch :: StringParsec u Signature
typeBranch = tuple <|> list <|> typeApplication

typeApplication :: StringParsec u Signature
typeApplication = do
    prefix <- symbol
    ((TypeApp prefix) <$> typeBranch) <|> return (Symbol prefix)

expr :: StringParsec u Signature
expr = do
    btype <- typeBranch
    (Function btype <$> (string "->" *> spaces *> expr) ) <|> return btype

list :: StringParsec u Signature
list = TypeApp "[]" <$> braces expr

tuple :: StringParsec u Signature
tuple = do
    elems <- (parens $ sepBy expr $ (char ',' *> spaces))
    case elems of
        [] -> return $ Symbol "()"
        [e] -> return e
        _ -> return $ Tuple elems

braces ::  StringParsec u Signature -> StringParsec u Signature
braces sub = (char '[' *> spaces) *> sub <* (char ']' *> spaces)

parens sub = (char '(' *> spaces) *> sub <* (char ')' *> spaces)

withEof :: StringParsec u Signature -> StringParsec u Signature
withEof content = content <* eof

parseUnknown :: StringParsec () Signature -> String -> Either ParseError Signature
parseUnknown e = parse (withEof e) "(unknown)"

parseSignature :: [Char] -> Either ParseError Signature
parseSignature = parseUnknown expr


instance IsString Signature where 
    fromString s = either (error . show) id $ parseSignature s
-- ----------------------

children :: Signature -> [Signature]
children = nub . children'
    where
    children' :: Signature -> [Signature]
    children' Symbol{} = []
    children' TypeApp {getPostfix = p} = [p]
    children' (Tuple e) = e ++ (concatMap children e)
    children' (Function param result) = param : result : children result


nextKey :: [(String, String)] -> String
nextKey [] = "a"
nextKey ((_,k):_) = nextKey' k
    where
    nextKey' :: String -> String
    nextKey' k = head $ tail $ dropWhile (/= k) $ (map return ['a'..'z']) ++ [x:y:[] | x <- ['a'..'z'], y <- ['a'..'z']]

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
parents s@Symbol{} = s
parents (TypeApp _  s@Symbol{}) = s
parents (TypeApp pre post)       = TypeApp pre $ parents post
parents (Tuple e) = Tuple $ map parents e
parents (Function param result) = Function (parents param) (parents result)


countComplex :: Signature -> Int
countComplex Symbol{} = 0
countComplex (TypeApp _ post)       = 1 + countComplex post
countComplex (Tuple e) = 1 +  (sum $ map countComplex e)
countComplex (Function param result) = 1 + countComplex param + countComplex result

isCompex :: Signature -> Bool
isCompex f@Function{} = countComplex f >= 3
isCompex _ = False

expand :: Signature -> [Signature]
expand s = s : (filter isCompex $ parents s : (parents $ parents s): children s)

expandNormalized :: Signature -> [Signature]
expandNormalized = map (fst . normalizeSignature) . expand