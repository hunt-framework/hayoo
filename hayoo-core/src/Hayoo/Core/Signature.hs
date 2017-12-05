{-# LANGUAGE OverloadedStrings #-}
module Hayoo.Core.Signature
  ( Signature
  , parse
  , print
  ) where


import           Data.Semigroup  ((<>))
import qualified Data.Text       as T
import           Prelude         hiding (print)
import qualified Text.Megaparsec as M



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
      parenthesize a <> print symbol <> parenthesize b

    TypeApp types ->
      T.intercalate " " (map parenthesize types)

    Tuple elements ->
      "(" <> T.intercalate ", " (map undefined elements) <> ")"

    Function parameter result ->
      parenthesize parameter <> "->" <> print result

    Context context type_ ->
      parenthesize context <> "=>" <> print type_

    Equiv left right ->
      print left <> "~" <> print right

    ExType locals type_ ->
      "forall " <> (T.intercalate " " (fmap print locals)) <> "." <> print type_


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
  = M.Parsec T.Text ()


parse :: T.Text -> Either (M.ParseError Char ()) Signature
parse =
  undefined


fullSignature :: Parser Signature
fullSignature =
  undefined


varSym :: Parser Signature
varSym =
  undefined



-- IDENTIFIER PREDICATES


isIdentifier :: Char -> Bool
isIdentifier c =
  undefined
