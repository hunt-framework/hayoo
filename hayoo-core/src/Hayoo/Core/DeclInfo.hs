{-# LANGUAGE OverloadedStrings #-}
module Hayoo.Core.DeclInfo
  ( DeclInfo (..)
  , DeclType (..)
  , docURI
  ) where


import           Data.Monoid ((<>))
import qualified Data.Text   as T



-- DECLARATION INFO


-- | A FunctionInfo stores additional information about a function
-- in Haskell.
data DeclInfo
    = DeclInfo
      { moduleName  :: T.Text
      -- ^ The name of the module containing the function, e.g. Data.Map

      , signature   :: Maybe T.Text
      -- ^ The full signature of the declaration, e.g. Ord a => a -> Int -> Bool

      , name        :: T.Text
      -- ^ The full name of this declaration e.g. DeclInfo

      , package     :: T.Text
      -- ^ The name of the package containing the module, e.g. containers

      , sourceURI   :: Maybe T.Text
      -- ^ An optional URI to the online source of the function.

      , description :: Maybe T.Text
      -- ^ The haddock description of a type or function, maybe shortened for space efficiency

      , declType    :: !DeclType
      -- ^ The type of the documented part, function, class, type, ...
      } deriving (Show, Eq)


data DeclType
  = Class
  | Data
  | Function
  | Method
  | Module
  | Newtype
  | Type
  | Unknown
  deriving (Show, Eq, Enum, Bounded)



-- PRIVATE URI HELPERS


hackageUri :: T.Text
hackageUri =
  "http://hackage.haskell.org/package"


packageUri :: DeclInfo -> T.Text
packageUri pkg =
  hackageUri </> (package pkg)


moduleUri :: DeclInfo -> T.Text
moduleUri info =
  packageUri info </> "docs" </> T.replace "." "-" (moduleName info) <> ".html"


functionUri :: DeclInfo -> T.Text
functionUri info =
  moduleUri info <> "#v:" <> name info


typeUri :: DeclInfo -> T.Text
typeUri info =
  moduleUri info <> "#t:" <> name info



-- PUBLIC HELPERS


-- | Compute the Hackage URI associated with this declaration.
--
-- There are several different types of URIs relevant for Hayoo!.
-- Here are examples for all of them, using Aeson as an
-- example library.
--
--     * /package/:  http://hackage.haskell.org/package/aeson
--     * /module/:   http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html
--     * /type/:     http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#t:Object
--     * /function/: http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#v:decode
--
-- If the @declType@ of a @DeclInfo@ is @Unkown@, the URI for the
-- module will be used.
docURI :: DeclInfo -> T.Text
docURI info =
  case declType info of
    Class ->
      typeUri info

    Data ->
      typeUri info

    Function ->
      functionUri info

    Method ->
      functionUri info

    Module ->
      moduleUri info

    Newtype ->
      typeUri info

    Type ->
      typeUri info

    Unknown ->
      moduleUri info



-- HELPERS


(</>) :: T.Text -> T.Text -> T.Text
(</>) a b =
  a <> "/" <> b
