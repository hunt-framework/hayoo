module Hayoo.ParseSignature where

import Text.ParserCombinators.Parsec
import Data.List


data Signature = 
      Identifier { getIdentifier :: String }
    | ComplexIdentifier { getPrefix :: String, getPostfix :: Signature }
    | Tuple { getElements :: [Signature] }
    | Function { getParameter :: Signature, getResult :: Signature }
    deriving (Eq)


instance Show Signature where
    show (Identifier i) = i
    show (ComplexIdentifier "[]" c) = "[" ++ (show c) ++ "]"
    show (ComplexIdentifier p c@(Tuple _)) = p ++ " " ++ (show c)
    show (ComplexIdentifier p c) = p ++ " (" ++ (show c) ++ ")"
    show (Tuple c) = "(" ++ (intercalate "," $ map show c) ++ ")"
    show (Function p r) = "(" ++ (show p) ++ "->" ++ (show r) ++ ")"

haskellIdentifier :: GenParser Char st String
haskellIdentifier = many1 $ (alphaNum <|> char '.')

identifier :: GenParser Char st Signature
identifier = fmap Identifier haskellIdentifier

complexIdentifier :: GenParser Char st Signature
complexIdentifier = do
    prefix <- haskellIdentifier
    many1 space
    postfix <- signature
    return $ ComplexIdentifier prefix postfix

signatureInParenthesis :: GenParser Char st Signature
signatureInParenthesis = do
    char '(' 
    spaces
    content <- signature
    spaces
    char ')'
    return $ content

list :: GenParser Char st Signature
list = do
    char '[' 
    spaces
    content <- signature
    spaces
    char ']'
    return $ ComplexIdentifier "[]" content

tuple :: GenParser Char st Signature
tuple = do
    char '(' 
    spaces
    c <- sepBy signature $ (spaces >> char ',' >> spaces)
    spaces
    char ')'
    return $ Tuple c


function :: GenParser Char st Signature
function = do
    param <- (try signatureInParenthesis) <|> tuple <|> list <|> (try complexIdentifier) <|> identifier
    spaces
    string "->"
    spaces
    result <- signature
    return $ Function param result

signature :: GenParser Char st Signature
signature = do
    c <- (try function) <|> (try signatureInParenthesis) <|> tuple <|> list <|> (try complexIdentifier) <|> identifier
    return c


withEof :: (GenParser Char st Signature) -> GenParser Char st Signature
withEof content = do
    c <- content
    eof
    return c

-- sinleElement = 

-- element = elementInParanthesis <|> 


-- elements = sepBy element (string "->")