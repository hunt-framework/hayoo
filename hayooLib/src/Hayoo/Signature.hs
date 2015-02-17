module Hayoo.Signature(
    Signature
  , parse
  , parseNormalized
  , pretty
  , normalize
  , explode
  , explodeNormalized
  , explodeWithComplexity
  , fromType
  , complexity
  ) where

import qualified Hayoo.Haskell as Haskell

import           Control.Applicative
import           Control.Monad.State
import           Data.Char (ord, chr)
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           Language.Haskell.Exts.Syntax
import qualified Numeric

newtype Signature = Signature { unSignature :: Type }
                  deriving (Eq, Ord, Show)

data Names = Names !(Map Name Name) !Word

normalize :: Signature -> Signature
normalize = Signature . normalize' . unSignature

normalize' :: Type -> Type
normalize' type_ =
  evalState (go type_) (Names Map.empty 0)
  where
    name :: Name -> State Names Name
    name n = do
      Names nameMap nextName <- get
      case Map.lookup n nameMap of
       Just x  -> return x
       Nothing -> do
         let name' = Ident (intToBase26 nextName)
         put (Names
              (Map.insert n name' nameMap)
              (succ nextName)
             )
         return name'
      where
        intToBase26 a =
          Numeric.showIntAtBase 26 (chr . (ord 'a' +)) a ""

    go tpe =
      case tpe of
       TyForall varbind ctx t -> TyForall
                                  <$> goVarbinds varbind
                                  <*> forM ctx goAsst
                                  <*> go t
       TyFun t1 t2            -> TyFun <$> go t1 <*> go t2
       TyTuple b tpx          -> TyTuple b <$> forM tpx go
       TyList t               -> TyList <$> go t
       TyParArray t           -> TyParArray <$> go t
       TyApp t1 t2            -> TyApp <$> go t1 <*> go t2
       TyVar n                -> TyVar <$> name n
       TyParen t              -> TyParen <$> go t
       TyInfix t1 qname t2    -> TyInfix
                                  <$> go t1
                                  <*> pure qname
                                  <*> go t2
       TyKind t kind          -> TyKind <$> go t <*> pure kind
       TyEquals t1 t2         -> TyEquals <$> go t1 <*> go t2
       TyBang _ t             -> go t
       x                      -> return x

    goVarbinds (Just varbinds) = Just <$> forM varbinds goVarbind
    goVarbinds Nothing         = return Nothing

    goVarbind (KindedVar n kind) = KindedVar <$> name n <*> pure kind
    goVarbind (UnkindedVar n)    = UnkindedVar <$> name n

    goAsst (ClassA qname types) = ClassA qname <$> forM types go
    goAsst (VarA n)             = VarA <$> name n
    goAsst (InfixA t1 qname t2) = InfixA <$> go t1 <*> pure qname <*> go t2
    goAsst (IParam ipname t)    = IParam ipname <$> go t
    goAsst (EqualP t1 t2)       = EqualP <$> go t1 <*> go t2
    goAsst (ParenA asst)        = ParenA <$> goAsst asst

complexity :: Signature -> Int
complexity = go . unSignature
  where
    go (TyForall binds _ t) = Maybe.maybe 0 List.length binds + go t
    go (TyCon _)            = 1
    go (TyApp t1 t2)        = 0 + go t1 + go t2
    go (TyTuple _ t)        = 1 + sum (map go t)
    go (TyList t)           = 1 + go t
    go (TyFun t1 t2)        = 1 + go t1 + go t2
    go (TyEquals t1 t2)     = 1 + go t1 + go t2
    go (TyParen t)          = 0 + go t
    go _                    = 0

explodeWithComplexity :: Int -> Signature -> Set Signature
explodeWithComplexity c  =
  Set.filter ((c <=) . complexity) . explodeNormalized

explode :: Signature -> Set Signature
explode = Set.map Signature . explode' . unSignature

explode' :: Type -> Set Type
explode' type_ =
  case type_ of
   for@(TyForall _ _ t)  -> set for `mappend` explode' t
   fun@(TyFun t1 t2)     -> set fun `mappend` explode' t1 `mappend` explode' t2
   tup@(TyTuple _ tpx)   -> set tup `mappend` mconcat (fmap explode' tpx)
   lis@(TyList t)        -> set lis `mappend` explode' t
   par@(TyParArray t)    -> set par `mappend` explode' t
   app@(TyApp t1 t2)     -> set app `mappend` explode' t1 `mappend` explode' t2
   par@(TyParen t)       -> set par `mappend` explode' t
   inf@(TyInfix t1 _ t2) -> set inf `mappend` explode' t1 `mappend` explode' t2
   kin@(TyKind t _)      -> set kin `mappend` explode' t
   equ@(TyEquals t1 t2)  -> set equ `mappend` explode' t1 `mappend` explode' t2
   ban@(TyBang _ t)      -> set ban `mappend` explode' t
   x                     -> set x
  where
    set = Set.singleton

explodeNormalized :: Signature -> Set Signature
explodeNormalized =
  Set.map Signature . explodeNormalized' . unSignature

explodeNormalized' :: Type -> Set Type
explodeNormalized' = Set.map normalize' . explode'

parse :: String -> Either String Signature
parse s = Signature <$> Haskell.parse s

parseNormalized :: String -> Either String Signature
parseNormalized s = normalize <$> parse s

pretty :: Signature -> String
pretty = Haskell.pretty . unSignature

fromType :: Type -> Signature
fromType = Signature
