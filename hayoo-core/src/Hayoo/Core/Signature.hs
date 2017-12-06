{-# LANGUAGE OverloadedStrings #-}
module Hayoo.Core.Signature
  ( Signature
  , parse
  , print
  ) where


import           Control.Applicative                   ((<|>))
import qualified Control.Applicative                   as A
import qualified Data.Char.Properties.UnicodeCharProps as Char
import           Data.Semigroup                        ((<>))
import qualified Data.Text                             as T
import           Data.Void
import           Prelude                               hiding (print)
import qualified Text.Megaparsec                       as M
import qualified Text.Megaparsec.Char                  as M



-- SIGNATURE


data Signature
  = VarSym                       -- | Symbol for a variable type, e.g `a`
    { _varName :: T.Text
    }
  | TypeSym                      -- | Symbol for a concrete type, e.g `Int`
    { _typeName :: T.Text
    }
  | InfixTypeApp                 -- | Symbol for an infix type, e.g. a :+: b
    { _infixSym   :: Signature
    , _infixTypeA :: Signature
    , _infixTypeB :: Signature
    }
  | ListType Signature           -- | Symbol for a list type, e.g. [a]
  | TypeApp                      -- | Symbol for type application, e.g. `Set a`
    { _types :: [Signature]
    }
  | Tuple                        -- | Symbol for a tuple type, e.g. (a, b)
    { _elements :: [Signature]
    }
  | Function                     -- | Symbol for a function, e.g. (a -> b)
    { _parameter :: Signature
    , _result    :: Signature
    }
  | Context                      -- | Signature for Contexts, e.g. (Monad m) => m a
    { _context     :: Signature
    , _contextType :: Signature
    }
  | Equiv                        -- | Signature for e.g. Token ~ Char
    { _left  :: Signature
    , _right :: Signature
    }
  | ExType                       -- | Signature for an existential, e.g. forall a. (a -> String)
    { _locals :: [Signature]
    , _exType :: Signature
    } deriving (Show, Eq)



-- PRINTING A SIGNATURE


print :: Signature -> T.Text
print signature =
  case signature of
    VarSym varName ->
      varName

    TypeSym typeName ->
      typeName

    ListType type_ ->
      "[" <> print type_ <> "]"

    InfixTypeApp symbol a b ->
      parenthesize a <> " " <> print symbol <> " " <> parenthesize b

    TypeApp types ->
      T.intercalate " " (map parenthesize types)

    Tuple elements ->
      "(" <> T.intercalate ", " (map parenthesize elements) <> ")"

    Function parameter result ->
      parenthesize parameter <> " -> " <> print result

    Context ctx type_ ->
      parenthesize ctx <> " => " <> print type_

    Equiv left right ->
      print left <> " ~ " <> print right

    ExType locals type_ ->
      "forall " <> (T.intercalate " " (fmap print locals)) <> ". " <> print type_


parenthesize :: Signature -> T.Text
parenthesize signature =
  case signature of
    VarSym _ ->
      print signature

    TypeSym _ ->
      print signature

    Tuple _ ->
      print signature

    ListType _ ->
      print signature

    InfixTypeApp _ _ _ ->
      print signature

    _ ->
      "(" <> print signature <> ")"



-- PARSING A SIGNATURE


type Parser
  = M.Parsec Void T.Text


parse :: T.Text -> Either (M.ParseError Char Void) Signature
parse =
  M.parse (expr <* M.eof) "<signature>"


expr :: Parser Signature
expr = do
  M.try function
    <|> M.try context
    <|> M.try equiv
    <|> M.try typeInfix


equiv :: Parser Signature
equiv =
  Equiv <$> typeInfix
        <*> (M.char '~' *> M.space *> expr)


context :: Parser Signature
context =
  Context <$> typeInfix
          <*> (ctxOp *> M.space *> expr)


function :: Parser Signature
function =
  Function <$> typeInfix
           <*> (fnOp *> M.space *> expr)


typeInfix :: Parser Signature
typeInfix =
  let
    typeOp t =
      InfixTypeApp <$> infixSym
                   <*> pure t
                   <*> typeInfix
  in do
    result <- typeApp
    typeOp result <|> pure result


typeApp :: Parser Signature
typeApp = do
  ts <- A.some primitive
  case ts of
    [t] ->
      pure t

    _ ->
      pure (TypeApp ts)


primitive :: Parser Signature
primitive =
  let
    varSym' = do
      sym <- varSym
      case sym of
        VarSym "forall" ->
          existential

        _ ->
          pure sym
  in
    typeSym <|> varSym' <|> tuple <|> list


tuple :: Parser Signature
tuple = do
  result <- between '(' (M.sepBy expr (M.char ',' *> M.space)) ')'
  case result of
    [] ->
      pure (TypeSym "()")

    [e] ->
      pure e

    _ ->
      pure (Tuple result)


list :: Parser Signature
list =
  ListType <$> between '[' expr ']'


existential :: Parser Signature
existential =
  ExType <$> A.some varSym
         <*> (M.char '.' *> M.space *> expr)



-- PRIMITIVE PARSERS


varSym :: Parser Signature
varSym =
  VarSym <$> varIdent <* M.space


typeSym :: Parser Signature
typeSym =
  TypeSym <$> typeIdent <* M.space


infixOpSym :: Parser Signature
infixOpSym =
  TypeSym <$> infixOp <* M.space


infixIdentSym :: Parser Signature
infixIdentSym =
  let
    sym =
      TypeSym <$> typeIdent <|> VarSym <$> varIdent
  in
    M.char '`' *> sym <* M.char '`' <* M.space


infixSym :: Parser Signature
infixSym =
  infixOpSym <|> infixIdentSym


between :: Char -> Parser a -> Char -> Parser a
between left p right =
  M.char left *> M.space *> p <* M.char right <* M.space


fnOp :: Parser T.Text
fnOp =
  M.string "->" <|> M.string "\8594"


ctxOp :: Parser T.Text
ctxOp =
  M.string "=>" <|> M.string "\8658"



-- IDENTIFIER PARSERS


varIdent :: Parser T.Text
varIdent =
  T.cons <$> M.satisfy (\c -> Char.isUnicodeLl c || c == '_') -- Must start with a lowercase letter
         <*> M.takeWhileP (Just "identifier") isIdentChar


typeIdent :: Parser T.Text
typeIdent =
  T.cons <$> M.satisfy (\c -> Char.isUnicodeLu c || Char.isUnicodeLt c)
         <*> M.takeWhileP (Just "type identifier") isQualifiedIdentChar


infixOp :: Parser T.Text
infixOp =
  T.cons <$> M.char ':'
         <*> M.takeWhileP (Just "infix type operator") isSymbolChar



-- IDENTIFIER AND SYMBOL PREDICATES


isSymbolChar :: Char -> Bool
isSymbolChar c =
  (c < '\128' && c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String))
    || (c >= '\128' && (Char.isUnicodeS c || Char.isUnicodeP c))


isQualifiedIdentChar :: Char -> Bool
isQualifiedIdentChar c =
  isIdentChar c
    || c == '.' -- for qualified names


isIdentChar :: Char -> Bool
isIdentChar c =
  isLetter c
    || c == '_'
    || c == '\''


isLetter :: Char -> Bool
isLetter c =
  Char.isUnicodeLl c       -- lowercase letter
    || Char.isUnicodeLu c  -- uppercase letter
    || Char.isUnicodeLt c  -- titlecase letter
    || Char.isUnicodeN c   -- digit
