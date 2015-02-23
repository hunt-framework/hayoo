{-# LANGUAGE OverloadedStrings #-}
module Hayoo.Query (
    hayooQuery
  , isSignatureQuery
  ) where

import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Hayoo.Signature (Signature)
import qualified Hayoo.Signature as Signature
import           Hunt.ClientInterface

hayooQuery :: Text -> Query
hayooQuery q = qOrs (concat [stdq, sigq, defq])
  where
    isSig = isSignatureQuery q

    sig :: [Signature]
    sig = ( if isSig
            then id
            else complexSignatures 3
          )                            -- throw away too simple queries
          . either (const []) ((:[]))  -- throw away parser errors
          . Signature.parseNormalized  -- try to parse q as signature
          . Text.unpack
          $ removeQuotes q

    subSigs :: [Signature]
    subSigs = concatMap (complexSignatures 1 . Set.toList . Signature.explode) sig

    subSigq :: [Query]
    subSigq
      | null subSigs = []
      | otherwise    = (:[])
                       . setBoost 0.1    -- results of sub signature queries have a reduced score
                       . setContexts ["subsig"]
                       . qAnds
                       . map ( qWord  -- exact case sensitive word search
                               . Text.pack -- sub signatures must be found in sub signature context
                               . Signature.pretty
                             )
                       $ subSigs

    sig1q :: [Query]
    sig1q = map ( setContexts ["signature"]
                  . qWord             -- case sensitive prefix search
                  . Text.pack         -- convert to Text
                  . Signature.pretty  -- convert Signature into String
                ) sig

    sigq :: [Query]
    sigq = map (\ q' -> qOrs $ q' : subSigq) sig1q

    stdq :: [Query]
    stdq
      | isSig     = []
      | otherwise = either (const []) (:[])      -- throw away errors
                    $ parseQuery (Text.unpack q) -- try to parse q as hunt query

    defq :: [Query]
    defq
      | null sigq                          -- if both query parsers fail,
        &&
        null stdq = [ qAnds                -- build a default query (AND)
                      . map qWordNoCase
                      . map removeQuotes
                      $ Text.words q       -- from the list of words
                    ]
      | otherwise = []


complexSignatures :: Int -> [Signature] -> [Signature]
complexSignatures c = filter ((c <=) . Signature.complexity)

isSignatureQuery :: Text -> Bool
isSignatureQuery q =
  "->" `Text.isInfixOf` q || "=>" `Text.isInfixOf` q

removeQuotes :: Text -> Text
removeQuotes t
  | Text.null t        = t
  | Text.head t == '"'
    &&
    Text.last t == '"' = Text.dropAround (== '"') t
  | Text.head t == '\''
    &&
    Text.last t == '\'' = Text.dropAround (== '\'') t
  | otherwise           = t
