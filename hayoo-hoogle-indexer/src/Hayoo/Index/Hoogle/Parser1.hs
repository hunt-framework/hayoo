{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
module Hayoo.Index.Hoogle.Parser1 where

import           Control.Applicative
import qualified Data.List as List
import           Hayoo.Haskell
import           Hayoo.Signature (fromType, Signature)
import           Language.Haskell.Exts.Syntax

data Fact = FactModule String
          | FactComment String
          | FactDecl   Decl
          deriving (Show)

data Inst a where
  ICommented :: String -> Inst a -> Inst a
  IModule    :: String -> Inst a -> Inst a
  IFact      :: Fact -> Inst Fact

type LineNumber = Int

type Error = String

data Line a = Line { lineNo ::  LineNumber, lineValue ::  a }
              deriving (Functor)

instance Show a => Show (Line a) where
  show (Line i a) = show i ++ ":" ++ show a

mergeComments :: [Line Fact] -> [Line Fact]
mergeComments [] = []
mergeComments (cmm1:cmm2:cx) | consecutiveComments = mergeComments (cmm:cx)
  where
    Line i1 t1 = cmm1
    Line i2 t2 = cmm2
    text (FactComment t) = t
    text _               = "mergeComments: not a comment"
    isComment (FactComment _) = True
    isComment _               = False
    cmm = Line i2 (FactComment (text t1 ++ "\n" ++ text t2))
    consecutiveComments = isComment t1 && isComment t2 && i1 + 1 == i2
mergeComments (c:cx) = c : mergeComments cx

annotate :: [Fact] -> [Inst Fact]
annotate = go IFact
  where
    go _   []     = []
    go ann (f:fx) =
      case f of
        FactComment c -> go (ICommented c . ann) fx
        FactModule m  -> ann (FactModule m) : go (IModule m . ann) fx
        _             -> ann f : go ann fx

factDescription :: Inst Fact -> String
factDescription (ICommented comment _) = comment
factDescription (IModule _ fact)       = factDescription fact
factDescription _                      = ""

factSignature :: Inst Fact -> Maybe Signature
factSignature (ICommented _ fact)      = factSignature fact
factSignature (IModule _ fact)         = factSignature fact
factSignature (IFact (FactDecl decl))  =
  case decl of
    TypeDecl _ _ _ t -> return (fromType t)
    TypeSig _ _ t    -> return (fromType t)
    _                -> Nothing
factSignature _  = Nothing

factName :: Inst Fact -> String
factName (ICommented _ fact)  = factName fact
factName (IModule _ fact)     = factName fact
factName (IFact fact)         =
  case fact of
    FactModule m  -> m
    FactDecl decl ->
      case decl of
        TypeSig _ [name] _           -> pretty name
        TypeDecl _ name _ _          -> pretty name
        GDataDecl _ _ _ name _ _ _ _ -> pretty name
        DataDecl _ _ _ name _ _ _    -> pretty name
        ClassDecl _ _ name _ _ _     -> pretty name
        _                            -> ""
    FactComment _ -> ""

factModule :: Inst Fact -> String
factModule (ICommented _ fact) = factModule fact
factModule (IModule m _)       = m
factModule (IFact _)           = ""

factType :: Inst Fact -> String
factType (IModule _ fact) = factType fact
factType (ICommented _ fact) = factType fact
factType (IFact fact) =
  case fact of
    FactModule _  -> "module"
    FactComment _ -> ""
    FactDecl decl -> case decl of
      TypeSig _ _ _          -> "function"
      TypeDecl _ _ _ _       -> "type"
      ClassDecl _ _ _ _ _ _  -> "class"
      DataDecl _ _ _ _ _ _ _ -> "data"
      _                      -> "unkown"

haddockAnchor :: Inst Fact -> String
haddockAnchor (IModule _ fact)    = haddockAnchor fact
haddockAnchor (ICommented _ fact) = haddockAnchor fact
haddockAnchor (IFact fact)        = case fact of
  FactModule _ -> ""
  FactComment _ -> ""
  FactDecl decl -> case decl of
    TypeSig _ _ _          -> "v:" ++ factName (IFact fact)
    DataDecl _ _ _ _ _ _ _ -> "v:" ++ factName (IFact fact)
    _                      -> "t:" ++ factName (IFact fact)

parseHoogle :: String -> ([Inst Fact], [Line Error])
parseHoogle content =
  let (facts, errors) = parse content in
  let insts           = annotate (fmap lineValue (mergeComments facts)) in
  (insts, errors)
  where
    parse = parseHoogle' . List.zip [1..] . List.lines

parseHoogle' :: [(LineNumber, String)] -> ([Line Fact], [Line Error])
parseHoogle' = go id id
  where
    go decls errors []   = (decls [], errors [])
    go decls errors ((_, []):lx) = go decls errors lx
    go decls errors  ((i, t):lx)
      | startsWith "-- "       = go (decls . ([Line i (comment t)] ++)) errors lx
      | startsWith "module "   = go (decls . ([Line i (module_ t)] ++)) errors lx
      | startsWith "instance " = go decls errors lx
      | startsWith "@"         = go decls errors lx
      | otherwise              =
        case parseDecl t of
          Left err      -> go decls (errors . ([Line i err] ++)) lx
          Right typeSig -> go (decls . ([Line i typeSig] ++)) errors lx
      where
        startsWith p = p `List.isPrefixOf` t

        comment s | "-- | " `List.isPrefixOf` s = FactComment (List.drop 5 s)
                  | otherwise                   = FactComment (List.drop 3 s)

        module_ = FactModule . List.drop 7

unGADT :: Decl -> Decl
unGADT (GDataDecl a b c d e _ [] f) = DataDecl a b c d e [] f
unGADT x = x

parseDecl :: String -> Either String Fact
parseDecl s | Right decl <- parse s = Right (FactDecl (unGADT decl))
parseDecl s | "class " `List.isPrefixOf` s = do
  x <- parse $ unwords (takeWhile ("where" /=) (words s)) ++ " where x :: Int"
  return (FactDecl x)
parseDecl s | "newtype " `List.isPrefixOf` s = do
  DataDecl a _ c d e f g <- unGADT <$> parse ("data " ++ List.drop 7 s)
  return (FactDecl (DataDecl a NewType c d e f g))
parseDecl s | Right (GDataDecl _ _ _ _ _ _ [GadtDecl a name _ ty] _) <- parse ("data Data where " ++ s) =
  return (FactDecl (TypeSig a [name] ty))
parseDecl s = Left $ "unknown decl: " ++ s
