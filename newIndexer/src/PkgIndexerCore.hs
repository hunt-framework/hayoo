module PkgIndexerCore
where

import           PackageInfo     (PackageInfo(..))
import           Data.List       (intercalate)
import           JsonUtil        (UTCTime, fmtDateXmlSchema, fmtDateHTTP
                                 ,pair, APair, Value, toJSON, object, fullWord)

-- | Build the index for a PackageInfo (except for the "indexed" value)
buildIndexPairs :: PackageInfo                  -- ^ PackageInfo record
                -> [APair]                      -- ^ Pairs comprising the index for this document
buildIndexPairs pkgInfo = kvpairs
  where
    kvpairs =
      -- description, author, category, indexed, name, synopsis, type, dependencies
      [ pair "description"  (p_description pkgInfo)
      , pair "author"       (p_author pkgInfo)
      , pair "category"     (p_category pkgInfo)
      , pair "name"         (p_name pkgInfo)
      , pair "synopsis"     (p_synopsis pkgInfo)
      , pair "type"         "package"
      , pair "dependencies" (intercalate " " (p_dependencies pkgInfo))
      ]

-- | Build the document component of an Insert command.
buildDocument :: UTCTime         -- ^ The indexed time
              -> PackageInfo     -- ^ The PackageInfo record
              -> Value           -- ^ Document object (as JSON)
buildDocument now pkgInfo =
  object  $
  [ pair "description"
         (object $ [ -- description, author, category, dependencies, indexed, maintainer, name, synopsis
                     -- type, version
                     pair "indexed"      nowD
                   , pair "description"  (p_description pkgInfo)
                   , pair "author"       (p_author pkgInfo)
                   , pair "category"     (p_category pkgInfo)
                   , pair "dependencies" (p_dependencies pkgInfo)
                   , pair "maintainer"   (p_maintainer pkgInfo)
                   , pair "name"         (p_name pkgInfo)
                   , pair "synopsis"     (p_synopsis pkgInfo)
                   , pair "version"      (p_version pkgInfo)
                   ]
         )
  , pair "index"       index
  , pair "uri"         uri
  ]
  where
    nowD     = toJSON $ fmtDateHTTP now       -- date formatted for the document
    nowI     = toJSON $ fmtDateXmlSchema now  -- date formatted for the index
    uri      = "http://hackage.haskell.org/package/" ++ (p_name pkgInfo)
    index    = object $ [ pair "indexed" nowI ] ++ buildIndexPairs pkgInfo

-- | Build the Insert command for a package.
buildInsert now pkgInfo =
  object $ [ pair "cmd"       "insert"
           , pair "document"  (buildDocument now pkgInfo)
           ]

-- | Build the Delete command for a list of packages.
buildDeletes :: [String] -> Value
buildDeletes pkgNames =
  object
  [ pair "cmd"   "delete-by-query"
  , pair "query" (object [ pair "args" [arg1, arg2]
                         , pair "op"   "and"
                         , pair "type" "seq"
                         ]
                 )
  ]
  where arg1 = object [ pair "type"    "context"
                      , pair "contexts" [ "type" ]
                      , pair "query"    (fullWord "package")
                      ]
        arg2 = object [ pair "type"     "context"
                      , pair "contexts" [ "name" ]
                      , pair "query"    (object [ pair "args" (map fullWord pkgNames)
                                                , pair "op"   "or"
                                                , pair "type" "seq"
                                                ]
                                        )
                      ]


