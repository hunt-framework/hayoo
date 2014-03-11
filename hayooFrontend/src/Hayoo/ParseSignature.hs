module Hayoo.ParseSignature where

import Text.Parsec
import Data.List

import Control.Monad.Identity
import Control.Applicative    ((*>), (<*), (<$>))

data Signature = 
      Symbol { getSymbol :: String }
    | TypeApp { getPrefix :: String, getPostfix :: Signature }
    | Tuple { getElements :: [Signature] }
    | Function { getParameter :: Signature, getResult :: Signature }
    deriving (Eq)


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

