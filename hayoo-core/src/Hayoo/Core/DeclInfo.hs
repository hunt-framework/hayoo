module Hayoo.Core.DeclInfo
  ( DeclInfo (..)
  , DeclType (..)
  ) where


import qualified Data.Text as T



-- FUNCTION INFO


-- | A FunctionInfo stores additional information about a function
-- in Haskell.
data DeclInfo
    = DeclInfo
      { moduleName :: T.Text
      -- ^ The name of the module containing the function, e.g. Data.Map

      , signature  :: T.Text
      -- ^ The full signature of the function, e.g. Ord a => a -> Int -> Bool

      , package    :: T.Text
      -- ^ The name of the package containing the module, e.g. containers

      , sourceURI  :: T.Text
      -- ^ An optional URI to the online source of the function.

      , declDescr  :: T.Text
      -- ^ The haddock description of a type or function, maybe shortened for space efficiency

      , declType   :: !DeclType
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
