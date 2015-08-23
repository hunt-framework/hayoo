module FctIndexerCore
where

import           PackageInfo     (PackageInfo(..))
import           Data.List       (intercalate)
import           JsonUtil        (UTCTime, fmtDateXmlSchema, fmtDateHTTP
                                 ,pair, APair, Value, toJSON, object, fullWord)

import           HoogleProcessLine (fixupSignature)
import           Hayoo.ParseSignature (parseSignature, prettySignature, complexSignatures, subSignatures)
import           Hayoo.FunctionInfo
import           ParseHoogle (removeTags)

optPair :: String -> String -> [ APair ]
optPair k [] = []
optPair k v  = [ pair k v ]

-- | Build the index for a FunctionInfo
buildIndexPairs :: String                       -- ^ Name of function / type / module / etc.
                -> FunctionInfo                 -- ^ FunctionInfo record
                -> [APair]                      -- ^ Pairs comprising the index for this document
buildIndexPairs fctName fctInfo = kvpairs
  where
    kvpairs =
      [ pair "package"     (package fctInfo)
      , pair "module"      mname
      , pair "name"        fctName
      , pair "type"        infoType
      , pair "hierarchy"   hierarchy
      ]
      ++ optPair "description" descrWords
      ++ sigPairs

    infoType = fromFct'Type (fctType fctInfo)

    mname = moduleName fctInfo

    hierarchy = map replaceDotSpace mname
      where
        replaceDotSpace '.' = ' '
        replaceDotSpace x   = x

    sigPairs =
      let sig = signature fctInfo in
      if null sig then []
                  else [ pair "signature" sig
                       , pair "subsig"    (toSubSignatures sig)
                       ]
    descrWords = removeTags (fctDescr fctInfo)

-- | Build the document component of an Insert command.
buildDocument :: [APair]         -- ^ Score key-value pair for this package (or null)
              -> UTCTime         -- ^ The indexed time
              -> String          -- ^ The function / method / type name
              -> FunctionInfo    -- ^ The FunctionInfo record
              -> Value           -- ^ Document object (as JSON)
buildDocument scoreKV now fctName fctInfo =
    object  $
    [ pair "description"
           ( object $ [ pair "indexed"     nowD
                      , pair "package"     (package fctInfo)
                      , pair "module"      (moduleName fctInfo)
                      , pair "name"        fctName
                      , pair "type"        infoType
                      , pair "source"      (sourceURI fctInfo)
                      ]
                      ++ optPair "description" (fctDescr fctInfo)
           )
    , pair "index" index
    , pair "uri"   (docURI fctInfo)
    ]
    ++ scoreKV
  where
    nowD     = toJSON $ fmtDateHTTP now       -- date formatted for the document
    nowI     = toJSON $ fmtDateXmlSchema now  -- date formatted for the index
    infoType = fromFct'Type (fctType fctInfo)
    index    = object $ [ pair "indexed" nowI ] ++ buildIndexPairs fctName fctInfo

buildInsert :: [APair]          -- ^ Score Pair (or null) for this package
            -> UTCTime          -- ^ The index time
            -> String           -- ^ The function / method / type name
            -> FunctionInfo     -- ^ The FunctionInfo record
            -> Value            -- ^ Insert command (as JSON)
buildInsert scoreKV now fctName fctInfo =
  object $ [ pair "cmd"      "insert"
           , pair "document" (buildDocument scoreKV now fctName fctInfo)
           ]
    
-- | Build the Insert commands for a list of FunctionInfo records.
buildInserts :: Maybe Score -> UTCTime -> [ (String, FunctionInfo) ] -> [Value]
buildInserts score now items = [ buildInsert scoreKV now fctName fctInfo | (fctName, fctInfo) <- items ]
  where
    scoreKV = maybe []  (\s -> [ pair "weight" s ] ) score

-- | Build the Delete command for a package.
buildDelete :: String -> Value
buildDelete pkgName =
  object
  [ pair "cmd"  "delete-by-query"
  , pair "query" ( object [ pair "type"     "context"
                          , pair "contexts" [ "package" ]
                          , pair "query"    (fullWord pkgName)
                          ]
                 )
  ]

-- signature stuff

toSubSignatures :: String -> String
toSubSignatures str =
  case parseSignature (fixupSignature str) of
    Left _    -> ""
    Right sig -> intercalate "\n" $ map prettySignature $ complexSignatures 1 $ subSignatures sig

