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

    , processSignatureWith
    , parseNormSignature
    , subSignatures
    , normSignatures
    , normSignature
    , isComplex
    , complexSignatures
    , buildConstructorSignature  -- for Hayoo indexer
    )
where

import           Control.Applicative                   ((*>), (<$>), (<*),
                                                        (<*>))
import           Control.Monad.Identity                (Identity)
import           Control.Monad.State                   (State, get, put,
                                                        runState)
import           Data.Char.Properties.UnicodeCharProps (isUnicodeLl,
                                                        isUnicodeLt,
                                                        isUnicodeLu, isUnicodeN,
                                                        isUnicodeP, isUnicodeS)
import           Data.Foldable                         (Foldable)
import           Data.List                             (intercalate, nub)
import           Data.String                           (IsString, fromString)
import           Data.Traversable                      (Traversable, mapM)
import           Prelude                               hiding (mapM, sequence)
import           Text.Parsec                           (ParseError, ParsecT,
                                                        char, eof, many, many1,
                                                        parse, satisfy, sepBy,
                                                        spaces, string, (<?>),
                                                        (<|>))

-- ------------------------------------------------------------

data SignatureT a =
      VarSym    { getSymbol :: a }
    | TypeSym   { getTypeSym :: String }            -- String is important here (*)
    | TypeApp   { getTypes :: [SignatureT a] }
    | Tuple     { getElements :: [SignatureT a] }
    | Function  { getParameter :: SignatureT a,   getResult :: SignatureT a }
    | Context   { getContext   :: SignatureT a,   getType   :: SignatureT a }
    | Equiv     { getLeft      :: SignatureT a,   getRight  :: SignatureT a }
    | ExType    { getLocals    :: [SignatureT a], getType   :: SignatureT a }
    deriving (Eq, Functor, Foldable, Traversable, Show)

-- (*) it changes the semantics of Functor and the other derived instances
-- when renaming variables in @normalizeSignature@ only the @VarSym@ nodes are
-- mapped, the @TypeSym@s remain constant

type Signature = SignatureT String

type StringParsec r = ParsecT String () Identity r

type SignatureParser = StringParsec Signature
type StringParser    = StringParsec String
type CharParser      = StringParsec Char

-- --------------------
-- Char parser

idChar' :: (Char -> Bool) -> CharParser
idChar' p
    = satisfy (\ c ->    isUnicodeLl c   -- lowercase letter
                      || isUnicodeLu c   -- uppercase letter
                      || isUnicodeLt c   -- titlecase letter
                      || isUnicodeN c    -- digit
                      || p c
                 )

idChar :: CharParser
idChar = idChar' (`elem` "_'")

idCharDot :: CharParser
idCharDot = idChar' (`elem` "_'.") -- "." is included for qualified names

symChar :: CharParser
symChar
    = satisfy
      (\ c -> ( c < '\128'
                && ( c `elem` "!#$%&*+./<=>?@\\^|-~"
                     || c == ':'
                   )
              )
              ||
              ( c >= '\128'
                && ( isUnicodeS c
                     || isUnicodeP c
                   )
              )
      )

-- --------------------
-- String parser

idSuffix :: StringParser
idSuffix = many idChar

idSuffixDot :: StringParser
idSuffixDot = many idCharDot

varId :: StringParser
varId = (:) <$> satisfy (\ c -> isUnicodeLl c || c == '_')
            <*> idSuffix

typeId :: StringParser
typeId = (:) <$> satisfy (\ c -> isUnicodeLu c || isUnicodeLt c)
             <*> idSuffixDot  -- type names may be qualified

conOp :: StringParser  -- infix type def, e.g "a :+: b"
conOp = (:) <$> char ':'
            <*> many symChar

-- --------------------
-- Signature parser

varSy :: SignatureParser
varSy = VarSym <$> (varId <* spaces)

typeSy :: SignatureParser
typeSy = TypeSym <$> (typeId <* spaces)

conSy :: SignatureParser
conSy = (TypeSym <$> (conOp <* spaces))
        -- <|>
        -- (VarSym  <$> (varOp <* spaces)) -- TODO: pretty of type variables as operators

infixIdSy :: SignatureParser
infixIdSy = char '`' *> ((TypeSym <$> typeId)
                         <|>
                         (VarSym  <$> varId )) <* (char '`' <* spaces)

infixSy :: SignatureParser
infixSy =     conSy
          <|> infixIdSy

prim :: SignatureParser
prim
    = typeSy <|> varSy' <|> tuple <|> list
      <?> "primitive type"
    where
      varSy'
          = do sy <- varSy
               case sy of
                 VarSym "forall"  -- check for reserved word "forall"
                     -> existentialType
                 _   -> return sy

typeApp :: SignatureParser
typeApp
    = do ts <- many1 prim
         case ts of
           [t] -> return t
           _   -> return $ TypeApp ts

typeInfix :: SignatureParser
typeInfix
    = do t1 <- typeApp
         typeOp t1 <|> return t1
    where
      typeOp t1
          = (\ op t2 -> TypeApp [op, t1, t2])
            <$> infixSy <*> typeApp  -- or typeInfix ?

expr :: SignatureParser
expr = do
    btype <- typeInfix -- exprBranch
    (     Function btype <$> (sarrow      *> spaces *> expr))
      <|> (Context btype <$> (darrow      *> spaces *> expr))
      <|> (Equiv   btype <$> (string "~"  *> spaces *> expr))
      <|> return btype
    where
      sarrow = string "->" <|> string "\8594"   -- unicode ->
      darrow = string "=>" <|> string "\8658"   -- unicode =>

list :: SignatureParser
list = (TypeApp . (TypeSym "[]" :) . (:[])) <$> brackets expr

tuple :: SignatureParser
tuple = do
    elems <- (parens $ sepBy expr $ (char ',' *> spaces))
    case elems of
        []  -> return $ TypeSym "()"
        [e] -> return e
        _   -> return $ Tuple elems

existentialType :: SignatureParser
existentialType
    = ExType <$> many1 varSy
             <*> (string "." *> spaces *> expr)

brackets ::  SignatureParser -> SignatureParser
brackets sub
    = (char '[' *> spaces) *> sub <* (char ']' *> spaces)

parens ::  StringParsec a -> StringParsec a
parens sub
    = (char '(' *> spaces) *> sub <* (char ')' *> spaces)

withEof :: SignatureParser -> SignatureParser
withEof content
    = content <* eof

parseSignature :: String -> Either ParseError Signature
parseSignature
    = parse (withEof expr) "<signature>"

instance IsString Signature where
    fromString s = either (error . show) id $ parseSignature s

-- ----------------------

isInfixType :: Signature -> Bool
isInfixType (TypeSym (':' : _)) = True
isInfixType _                   = False

addParens :: Signature -> String
addParens s                    = "(" ++ (pretty s) ++ ")"

checkParens :: Signature -> String
checkParens s@TypeSym{}                    = pretty s
checkParens s@VarSym{}                     = pretty s
checkParens s@Tuple{}                      = pretty s
checkParens s@(TypeApp (TypeSym "[]" : _)) = pretty s
checkParens s@(TypeApp (t:_))
    | isInfixType t                        = pretty s
checkParens s                              = addParens s

checkParensf :: Signature -> String
-- checkParensf s@(TypeApp (t:_))
--    | isInfixType t            = checkParens s
checkParensf s@(TypeApp _)
                               = pretty s
checkParensf s                 = checkParens s

pretty :: Signature -> String
pretty (TypeSym i)             = i
pretty (VarSym i)              = i
pretty (TypeApp [TypeSym "[]", c])
                               = "[" ++ (pretty c) ++ "]"
pretty (TypeApp [t, t1, t2])
    | isInfixType t            = checkParensf t1 ++ pretty t ++ checkParensf t2
pretty (TypeApp cs)            = intercalate " " $ map checkParens cs
pretty (Tuple cs)              = "(" ++ (intercalate "," $ map pretty cs) ++ ")"
pretty (Function p r)          = checkParensf p ++ "->" ++ pretty r
pretty (Context  c t)          = checkParensf c ++ "=>" ++ pretty t
pretty (Equiv    l r)          = pretty       l ++ "~"  ++ pretty r
pretty (ExType  ls t)          = "forall " ++ (intercalate " " $ map pretty ls) ++ "." ++ pretty t

prettySignature :: Signature -> String
prettySignature                = pretty

-- ----------------------

children :: Signature -> [Signature]
children = nub . children'
    where
    children' :: Signature -> [Signature]
    children' TypeSym{}              = []
    children' VarSym{}              = []
    children' (TypeApp cs)             = cs
    children' (Tuple e)                = e ++ (concatMap children' e)
    children' (Function param result)  = param : result : children' param ++ children' result
    children' (Context  cx    ty    )  =         ty     : cxElems cx      ++ children' ty
    children' (Equiv    l     r     )  = l     : r      : children' l     ++ children' r
    children' (ExType   _ls   ty    )  =         ty     : children' ty
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
      -- visit all VarSym nodes and rename ids into standard names
    where
      norm :: String -> State [(String, String)] String
      norm sym
          = do st <- get
               case lookup sym st of
                 Just s  -> return s
                 Nothing -> let s = nextKey st in
                            do put $ (sym, s) : st
                               return s

parents :: Signature -> Signature
parents s@TypeSym{}                = s
parents s@VarSym{}                 = s
parents (TypeApp [_, s@TypeSym{}]) = s                            -- drop the topmost type id
parents (TypeApp [_, s@VarSym{} ]) = s                            -- drop the topmost type id
parents (TypeApp cs)               = TypeApp $ parents <$> cs     -- simplify inner types
parents (Tuple e)                  = Tuple $ map parents e
parents (Function param result)    = Function (parents param) (parents result)
parents (Context  cx    ty    )    = Context  cx              (parents ty    )
parents e@Equiv{}                  = e
parents (ExType   ls    ty    )    = ExType ls                (parents ty    )

parents' :: Signature -> [Signature]
parents' s
    | s == s'   = []
    | otherwise = [s']
    where
      s' = parents s

countComplex :: Signature -> Int
countComplex VarSym{}                 = 0        -- var ids don't carry any information
countComplex TypeSym{}                = 1        -- type ids do
countComplex (TypeApp cs)             = 0 + (sum $ countComplex <$> cs)
countComplex (Tuple e)                = 1 + (sum $ map countComplex e)
countComplex (Function param result)  = 1 + countComplex param + countComplex result
countComplex (Context  cx    ty    )  = 1 + countComplex cx    + countComplex ty
countComplex (Equiv    l     r     )  = 1 + countComplex l     + countComplex r
countComplex (ExType   ls    ty    )  = length ls              + countComplex ty

isComplex :: Int -> Signature -> Bool
isComplex c s = countComplex s >= c

expand' :: Signature -> [Signature]
expand' s = ps1 ++ ps2 ++ children s
    where
      ps1 =           parents' s
      ps2 = concatMap parents' ps1

(<$$>) :: (Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)
(<$$>) f x = (fmap.fmap) f x

normSignatures :: [Signature] -> [Signature]
normSignatures
    = nub . concatMap normSignature

-- | Normalize names of var ids
--
-- If nothing is renamed, the empty list is returned

normSignature :: Signature -> [Signature]
normSignature s
    | noRename  = []
    | otherwise = [s']
      where
        (s', xs) = normalizeSignature s
        noRename = null $ filter (\ (x, y) -> x /= y) xs

-- ------------------------------------------------------------

-- | Compute all normalized sub signatures,
-- no filter by complexity is done, this can be done with 'complexSignatures'

subSignatures :: Signature -> [Signature]
subSignatures
  = nub . map (fst . normalizeSignature) . subs
  where
    subs (Context cx ty) = cxElems cx ++ [ty] ++ subs ty
    subs  Equiv{}        = []
    subs  s              = expand' s

-- | Filter signatures by complexity.
--
-- Every type name counts 1, every type construtor ("->", "[.]", "(,)", ...) counts 1.
-- All signatures with complexity @< c@ is removed

complexSignatures :: Int -> [Signature] -> [Signature]
complexSignatures c = filter (isComplex c)

-- | Parse a signature and rename variables by using a,b,c,...

parseNormSignature :: String -> Either ParseError Signature
parseNormSignature
    = either Left (Right . fst . normalizeSignature)
      . parse (withEof expr) "<signature>"

-- | Parse a signature, normalize it, transform it and convert is back to a string

processSignatureWith :: (Signature -> [Signature]) -> String -> String
processSignatureWith func sig
    = either (const "") (intercalate "\n") $ pretty <$$> (func <$> parseNormSignature sig)

buildConstructorSignature :: String -> String
buildConstructorSignature s
    = either (const "") editSig $ parseSignature ("XXX " ++ s)
      where
        editSig (TypeApp (_ : cs)) = pretty $ foldr1 Function cs
        editSig _                  = ""

-- ------------------------------------------------------------

expand :: Signature -> [Signature]
expand s = nub $ s : (filter (isComplex 3) $ expand' s)

{-# DEPRECATED expand "Use subSignatures instead" #-}

expandNormalized :: Signature -> [Signature]
expandNormalized = map (fst . normalizeSignature) . expand

{-# DEPRECATED expandNormalized "Use subSignatures, normSignature or normSignatures and complexSignatures instead" #-}

modifySignatureWith :: (Signature -> [Signature]) -> String -> String
modifySignatureWith func sig
    = either (const sig) (intercalate "\n") $ pretty <$$> (func <$> parseSignature sig)

{-# DEPRECATED modifySignatureWith "Use processSignatureWith instead" #-}

-- ------------------------------------------------------------

