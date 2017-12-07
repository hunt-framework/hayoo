{-# LANGUAGE OverloadedStrings #-}
module Hayoo.Indexer.Hoogle.Parse
  (
  ) where


import           Control.Applicative  ((<|>))
import qualified Control.Applicative  as A
import           Data.Semigroup       ((<>))
import qualified Data.Text            as T
import qualified Data.Void            as Void
import           Hayoo.Core.DeclInfo  (DeclInfo (..))
import qualified Hayoo.Core.DeclInfo  as DeclInfo
import qualified Text.Megaparsec      as M
import qualified Text.Megaparsec.Char as M



-- PARSE HOOGLE FORMAT


type Parser
  = M.Parsec Void.Void T.Text


data Package
  = Package
    { _comment :: Maybe T.Text
    , _name    :: T.Text        -- | e.g. @package servant
    , _version :: T.Text        -- | e.g. @version 0.12
    } deriving (Show, Eq)


parse :: T.Text -> Either (M.ParseError Char Void.Void) (Package, [DeclInfo])
parse =
  undefined



-- URI HELPERS


hackageUri :: T.Text
hackageUri =
  "http://hackage.haskell.org/package"


packageUri :: Package -> T.Text
packageUri pkg =
  hackageUri </> (_name pkg)


moduleUri :: Package -> T.Text -> T.Text
moduleUri pkg modName =
  packageUri pkg </> "docs" </> (T.replace "." "-" modName <> ".html")


functionUri :: DeclInfo -> T.Text -> T.Text
functionUri modInfo name =
  (docURI modInfo) <> "#v:" <> name


typeUri :: DeclInfo -> T.Text -> T.Text
typeUri modInfo name =
  (docURI modInfo) <> "#t:" <> name



-- CHAR HELPERS


isEndOfLine :: Char -> Bool
isEndOfLine c =
  c == '\n' || c == '\r'



-- PARSING


ident :: Parser T.Text
ident =
  M.takeWhileP (Just "identifier") (\c -> c == ' ' || isEndOfLine c)


line :: Parser T.Text
line =
  M.takeWhileP (Just "line") (not . isEndOfLine)
    <* (M.try M.eol <|> M.try (M.eof >> pure "") <|> pure "")


preamble :: Parser Package
preamble = do
  -- Skip the first few lines. They always look like the following:
  -- "-- Hoogle documentation, generated by Haddock\n
  --  -- See Hoogle, http://www.haskell.org/hoogle\n\n\n"
  line >> line >> M.space
  comment     <- A.optional docComment
  packageName <- M.string "@package " >> line
  version     <- M.string "@version " >> line
  pure (Package comment packageName version)


moduleInfo :: Package -> Parser DeclInfo.DeclInfo
moduleInfo pkg = do
  comment <- A.optional (M.try docComment)
  modName <- M.string "module " >> line
  pure $
    DeclInfo
      { moduleName = modName
      , signature  = ""                          -- No signature for a module
      , package    = (_name pkg)
      , sourceURI  = ""
      , declDescr  = comment
      , declType   = DeclInfo.Module
      , docURI     = moduleUri pkg modName
      }


functionInfo :: DeclInfo -> Parser DeclInfo
functionInfo modInfo = do
  comment <- A.optional (M.try docComment)
  fnName  <- ident
  sig     <- M.space >> line
  pure $
    DeclInfo
      { moduleName = moduleName modInfo
      , signature  = sig
      , package    = package modInfo
      , sourceURI  = ""
      , declDescr  = comment
      , declType   = DeclInfo.Function
      , docURI     = functionUri modInfo fnName
      }


typeInfo :: DeclInfo -> Parser DeclInfo
typeInfo modInfo = do
  comment  <- A.optional (M.try docComment)
  typeName <- M.string "type " >> ident -- TODO: Check whether this works
  lhs      <- M.takeWhileP (Just "left hand side") (/= '=')
  sig      <- M.char '=' >> line
  pure $
    DeclInfo
      { moduleName = moduleName modInfo
      , signature  = sig
      , package    = package modInfo
      , sourceURI  = ""
      , declDescr  = comment
      , declType   = DeclInfo.Type
      , docURI     = typeUri modInfo typeName
      }


newtypeInfo :: DeclInfo -> Parser DeclInfo
newtypeInfo modInfo = do
  comment     <- A.optional (M.try docComment)
  _           <- M.string "newtype "
  _constraint <- A.optional (M.try (M.manyTill M.anyChar (M.try (M.string " =>"))))
  typeName    <- ident -- TODO: Check whether this works for operators as well
  _params     <- line
  pure $
    DeclInfo
      { moduleName = moduleName modInfo
      , signature  = ""
      , package    = package modInfo
      , sourceURI  = ""
      , declDescr  = comment
      , declType   = DeclInfo.Newtype
      , docURI     = typeUri modInfo typeName
      }


dataInfo :: DeclInfo -> Parser DeclInfo
dataInfo modInfo = do
  comment <- A.optional (M.try docComment)
  _ <- M.string "data"
  undefined


instanceInfo :: DeclInfo -> Parser ()
instanceInfo modInfo = do
  _ <- A.optional (M.try docComment)
  _ <- M.string "instance "
  line >> pure ()


classInfo :: DeclInfo -> Parser ()
classInfo modInfo = do
  _ <- A.optional (M.try docComment)
  _ <- M.string "class "
  line >> pure ()



-- COMMENT PARSERS


docComment :: Parser T.Text
docComment = do
  firstLine <- M.string "-- | " >> line
  result    <- M.many (M.try (M.string "--   " >> line))
  pure (T.intercalate "\n" (firstLine:result))


lineComment :: Parser T.Text
lineComment = do
  M.string "-- " >> line


testPreamble :: T.Text
testPreamble =
  T.intercalate "\n"
  [ "-- Hoogle documentation, generated by Haddock"
  , "-- See Hoogle, http://www.haskell.org/hoogle/"
  , ""
  , ""
  , "-- | A family of combinators for defining webservices APIs"
  , "--   "
  , "--   A family of combinators for defining webservices APIs and serving them"
  , "--   "
  , "--   You can learn about the basics in the <a>tutorial</a>."
  , "--   "
  , "--   <a>CHANGELOG</a>"
  , "@package servant"
  , "@version 0.12"
  ]



-- HELPERS


(</>) :: T.Text -> T.Text -> T.Text
(</>) a b =
  a <> "/" <> b
