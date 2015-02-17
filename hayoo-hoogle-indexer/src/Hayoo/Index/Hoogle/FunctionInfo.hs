{-# LANGUAGE GADTs #-}
module Hayoo.Index.Hoogle.FunctionInfo where

import           Control.Monad
import           Data.Aeson (toJSON)
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Hayoo.Signature as Signature
import qualified Hunt.ClientInterface as Hunt
import qualified Hunt.Common.DocDesc as Hunt
import           Hayoo.Index.IndexSchema
import           Hayoo.Index.Hoogle.Parser1

type PackageName = String
type Version = String
type ModuleName = String
type Anchor = String

data FunctionInfo = FunctionInfo {
    fiDescription :: !Text
  , fiModule      :: !Text
  , fiName        :: !Text
  , fiPackage     :: !Text
  , fiSignature   :: !Text
  , fiSubsigs     :: !Text
  , fiType        :: !Text
  , fiVersion     :: !Text
  , fiURI         :: !Text
  } deriving (Show)

instance Hunt.Huntable FunctionInfo where
  huntURI      = fiURI
  huntDescr    = Hunt.mkDocDesc
                 . fmap toJSON
                 . HashMap.fromList
                 . toList
  huntIndexMap = Map.fromList . toList

type MkURI a = PackageName -> Version -> ModuleName -> a -> (a -> Anchor) -> String

toList :: FunctionInfo -> [(Text, Text)]
toList fi = filter (not . Text.null . snd) [
    c'description * fiDescription fi
  , c'module      * fiModule fi
  , c'name        * fiName fi
  , c'package     * fiPackage fi
  , c'signature   * fiSignature fi
  , c'subsig      * fiSubsigs fi
  , c'type        * fiType fi
  , c'version     * fiVersion fi
  ]
  where
    (*) = (,)

functionInfo :: MkURI (Inst Fact) -> PackageName -> Version -> [Inst Fact] -> [FunctionInfo]
functionInfo mkUri package version = concatMap (toFunctionInfo mkUri package version)

toFunctionInfo :: MkURI (Inst Fact)
               -> PackageName
               -> Version
               -> Inst Fact
               -> [FunctionInfo]
toFunctionInfo mkUri packageName version d =
  return FunctionInfo {
      fiURI         = Text.pack $ mkUri packageName version (factModule d) d haddockAnchor
    , fiDescription = Text.pack $ factDescription d
    , fiModule      = Text.pack $ factModule d
    , fiName        = Text.pack $ factName d
    , fiPackage     = Text.pack $ packageName
    , fiVersion     = Text.pack $ version
    , fiSignature   = Text.pack $ signature
    , fiSubsigs     = Text.pack $ subsignatures
    , fiType        = Text.pack $ factType d
    }
  where
    signature = maybe "" Signature.pretty $ do
      sig <- factSignature d
      return sig

    subsignatures =
      List.intercalate "\n" (mkSubsignatures signature)

mkSubsignatures :: String -> [String]
mkSubsignatures s = do
  signature    <- either (const mzero) return (Signature.parse s)
  subsignature <- Foldable.toList (Signature.explodeNormalized signature)
  return (Signature.pretty subsignature)
