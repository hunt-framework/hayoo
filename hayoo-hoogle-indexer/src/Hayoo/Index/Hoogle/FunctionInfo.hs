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
import qualified Hunt.Scoring.Score as Hunt
import           Hunt.Common.ApiDocument


import           Hayoo.Index.IndexSchema
import           Hayoo.Index.Hoogle.Parser1
import           Hayoo.Index.Hoogle.Types

data FunctionInfo = FunctionInfo {
    fiDescription      :: !Text
  , fiModule           :: !Text
  , fiName             :: !Text
  , fiPackage          :: !Text
  , fiSignature        :: !Text
  , fiDisplaySignature :: !Text
  , fiSubsigs          :: !Text
  , fiType             :: !Text
  , fiVersion          :: !Text
  , fiURI              :: !Text
  , fiRank             :: !Float
  } deriving (Show)

instance Hunt.Huntable FunctionInfo where
  huntURI      = fiURI
  huntDescr    = Hunt.mkDocDesc
                 . fmap toJSON
                 . HashMap.fromList
                 . toList
  huntIndexMap = Map.fromList . toList

  toApiDocument  x = ApiDocument
                      { adUri   = Hunt.huntURI x
                      , adIndex = Hunt.huntIndexMap x
                      , adDescr = Hunt.huntDescr x
                      , adWght  = Hunt.mkScore (fiRank x)
                      }

toList :: FunctionInfo -> [(Text, Text)]
toList fi = filter (not . Text.null . snd) [
    c'description * fiDescription fi
  , c'module      * fiModule fi
  , c'name        * fiName fi
  , c'package     * fiPackage fi
  , c'signature   * fiSignature fi
  , c'displaySignature * fiDisplaySignature fi
  , c'subsig      * fiSubsigs fi
  , c'type        * fiType fi
  , c'version     * fiVersion fi
  ]
  where
    (*) = (,)

functionInfo :: MkURI (Inst Fact)
             -> MkRank
             -> PackageName
             -> Version
             -> [Inst Fact]
             -> [FunctionInfo]
functionInfo mkUri mkRank package version =
  concatMap (toFunctionInfo mkUri mkRank package version)

toFunctionInfo :: MkURI (Inst Fact)
               -> MkRank
               -> PackageName
               -> Version
               -> Inst Fact
               -> [FunctionInfo]
toFunctionInfo mkUri mkRank packageName version d =
  return FunctionInfo {
      fiURI              = Text.pack $ mkUri packageName version (factModule d) d haddockAnchor
    , fiDescription      = Text.pack $ factDescription d
    , fiModule           = Text.pack $ factModule d
    , fiName             = Text.pack $ factName d
    , fiPackage          = Text.pack $ packageName
    , fiVersion          = Text.pack $ version
    , fiSignature        = Text.pack $ signature
    , fiDisplaySignature = Text.pack $ displaySignature
    , fiSubsigs          = Text.pack $ subsignatures
    , fiType             = Text.pack $ factType d
    , fiRank             = mkRank packageName
    }
  where
    displaySignature = maybe "" Signature.pretty $ do
      factSignature d

    signature = maybe "" Signature.pretty $ do
      sig <- factSignature d
      return (Signature.stripConstraints sig)

    subsignatures =
      List.intercalate "\n" (mkSubsignatures signature)

mkSubsignatures :: String -> [String]
mkSubsignatures s = do
  signature    <- either (const mzero) return (Signature.parse s)
  subsignature <- Foldable.toList (Signature.explodeNormalized signature)
  return (Signature.pretty subsignature)
