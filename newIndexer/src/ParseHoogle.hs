{-# LANGUAGE NoMonomorphismRestriction #-}

module ParseHoogle
where

import Control.Monad

import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

import Data.Char
import Data.List (isPrefixOf, break)

data HoogleLine = BlankLine
                | Comment String               -- comment line (begins with "--"
                | Package String               -- @package declaration
                | Version String               -- @version declaration
                | Module String                -- module ...
                | Type String String String    -- type <name> <params> = ...
                | Newtype String String        -- newtype <name> <params>
                | FunctionDecl String String   -- <name> :: <sig>   -- function signature
                | DataDecl String              -- data <name>
                | MultiDecl [String] String    -- (a,b,c) :: ...
                | BracketDecl [String] String  -- [a] :: ...
                | Instance String              -- instance (...) => ...
                | Class String                 -- class (...) => ...
                | DataType String String       -- dataType[...] :: DataType
                | Constr String String         -- constr[...] :: Constr
  deriving (Show)

isLineSpace c = isSpace c && c /= '\n'
lineSpace   = satisfy isLineSpace

lexeme p    = do{ x <- p; skipMany lineSpace; return x }

restOfLine  = many anyChar -- manyTill anyChar (char '\n');

symbol name = lexeme (string name)

identStart  = letter <|> char '_'
identLetter = alphaNum <|> oneOf "_'" <|> satisfy (\c -> ord c > 127)

ident = lexeme $ ((try ident') <?> "identifier")

-- an identifer without consuming following whitespace
ident' = do { c <- identStart
            ; cs <- many identLetter
            ; hash <- option False $ do { satisfy (== '#'); return True }
            ; return (c:cs)
            }

opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~" <|> satisfy (\c -> ord c > 127)

operator = lexeme $ (many1 opLetter) <|> tupleOp

parenOp = do char '('
             name <- many1 opLetter
             char ')'
             return name

tupleOp = do char '('
             commas <- many (symbol ",")
             char ')'
             return $ "(" ++ concat commas ++ ")"

identOrOp = ident <|> parenOp <|> operator

startsWith str =
  do symbol str
     restOfLine

instanceDef = fmap Instance $ startsWith "instance"
classDef    = fmap Class $ startsWith "class"
packageDef  = fmap Package $ startsWith "@package"
versionDef  = fmap Version $ startsWith "@version"
moduleDef   = fmap Module $ startsWith "module"

blankLine =
  do skipMany lineSpace
     eof
     return BlankLine

oneLineComment =
  do symbol "--";
     comment <- restOfLine
     return $ Comment comment

constraint = do
  manyTill anyChar (try (symbol " =>"))

newTypeDef = do
  symbol "newtype"
  option "" (try constraint)
  name <- identOrOp
  params <- restOfLine
  return $ Newtype name params

typeDef     = do
  symbol "type"
  name <- identOrOp
  lhs <- many (satisfy (/= '='))
  symbol "="
  sig <- restOfLine
  return $ Type name lhs sig

functionDecl = do
  name <- ident
  symbol "::"
  sig <- restOfLine
  return $ FunctionDecl name sig

-- data declarations examples:
--
--     data Scenario
--     data Lit s
--     data AbList a b
--     data SatResult :: *
--     data Network (l :: * -> *) (g :: * -> *) :: (* -> *) -> (* -> *) -> *
--     data (:=:) a b
--     data ATuple20 s[am5Q] a[am5R]
--     data DebuggerM (m :: * -> *) (past :: [*]) (current :: *) (future :: [*])
dataDef     = do
  symbol "data"
  try d1 <|> try d2 <|> d3
  where
    d1 = do name <- dataName
            params <- many dataParam
            kindsig <- (do eof; return "") <|> (do symbol "::"; restOfLine)
            return $ DataDecl name
    d2 = do skipMany1 ident
            symbol "=>"
            d1
    d3 = do lexeme $ do { char '('; many balancedParens; char ')' }
            symbol "=>"
            d1

dataName = lexeme $ try ident <|> try parenOp <|> tupleOp
dataParam = lexeme $ (try simpleParam <|> parenParam <|> dollarParam)
  where
    simpleParam = do i <- ident'; optional (do char '['; ident'; char ']'); return i
    parenParam = do char '('; many balancedParens; char ')'; return ""
    dollarParam = do char '$'; i <- ident'; return $ "$" ++ i

balancedParens =
  do { satisfy (\ch -> ch /= '(' && ch /= ')'); return () }
    <|> do { char '('; many balancedParens; char ')'; return () }

multiDecl =
  do names <- parenNames <|> (sepBy1 multiName (symbol ","))
     symbol "::"
     sig <- restOfLine
     return $ MultiDecl names sig
  where
    nakedOp = many1 opLetter
    multiName = lexeme (nakedOp <|> ident')
    parenNames = do
      symbol "("
      names <- sepBy multiName (symbol ",")
      symbol ")"
      return names

bracketDecl =
  do symbol "["
     names <- sepBy1 bracketName (symbol ",")
     symbol "]"
     symbol "::"
     sig <- restOfLine
     return $ BracketDecl names sig
  where
    bracketName = lexeme $ try ident <|> try parenOp <|> try tupleOp <|> nakedOp
    nakedOp = many1 opLetter

dataTypeDecl =
  do string "dataType"
     symbol "["
     name <- ident
     symbol "]"
     symbol "::"
     sig <- restOfLine
     return $ DataType name sig

constrDecl =
  do string "constr"
     symbol "["
     name <- ident
     symbol "]"
     symbol "::"
     sig <- restOfLine
     return $ Constr name sig

anyLine = try oneLineComment
          <|> try instanceDef
          <|> try classDef
          <|> try packageDef
          <|> try versionDef
          <|> try typeDef
          <|> try dataDef
          <|> try moduleDef
          <|> try newTypeDef
          <|> try functionDecl
          <|> try multiDecl
          <|> try bracketDecl
          <|> try dataTypeDecl
          <|> constrDecl
          <|> blankLine

hoogleLine = anyLine

-- | Naively remove HTML tags from a string.
removeTags :: String -> String
removeTags str =
  let (before,rest1) = break (== '<') str
      (tag,rest2)    = break (== '>') rest1
      rest3 = case rest2 of
                [] -> []
                (_:xs) -> xs
  in
  if null rest1
    then str
    else before ++ " " ++ removeTags rest3

