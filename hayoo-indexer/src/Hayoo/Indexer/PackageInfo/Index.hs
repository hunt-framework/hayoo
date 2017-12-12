{-# LANGUAGE OverloadedStrings #-}
module Hayoo.Indexer.PackageInfo.Index
  ( insert
  , delete
  ) where


import           Data.Aeson             as Json
import           Data.Semigroup         ((<>))
import qualified Data.Text              as T
import qualified Data.Time              as Time
import qualified Hayoo.Core.PackageInfo as PackageInfo
import qualified Hayoo.Indexer.Internal as I



-- INDEX PACKAGE INFO


-- | Build a command for inserting a @PackageInfo@ into
-- a Hunt index.
--
-- Here is an example, of how that would look like with the
-- @PackageInfo@ extracted from the hayoo-indexer file
-- (with some simplifications).
--
-- > {
-- >   "cmd": "insert",
-- >   "document": {
-- >     "uri": "http://hackage.haskell.org/package/hayoo-indexer",
-- >     "description": {
-- >       "indexed": "Mon Jan 01 10:00:00 2017",
-- >       "description": "An indexer, which turns hoogle and cabal files into hunt",
-- >       "author": "Matthias Metzger, Alex Biehl",
-- >       "category": "Web",
-- >       "dependencies": ["text", "aeson", "hayoo-core"],
-- >       "maintainer": "Matthias Metzger",
-- >       "name": "hayoo-indexer",
-- >       "synopsis": "",
-- >       "version": "0.1.0.0"
-- >     },
-- >     "index": {
-- >       "type": "package",
-- >       "indexed": "2017-01-01T10:00:00",
-- >       "description": "An indexer, which turns hoogle and cabal files into hunt",
-- >       "author": "Matthias Metzger, Alex Biehl",
-- >       "category": "Web",
-- >       "dependencies": "text aeson hayoo-core",
-- >       "name": "hayoo-indexer"
-- >     }
-- >   }
-- > }
insert :: Time.UTCTime -> PackageInfo.PackageInfo -> Json.Value
insert now pkg =
  let
    uri =
      "http://hackage.haskell.org/package/" <> (PackageInfo.pkgName pkg)

    description =
      Json.object
        [ "indexed"      .= I.fmtTime "%c" now
        , "description"  .= PackageInfo.pkgDescription pkg
        , "author"       .= PackageInfo.pkgAuthor pkg
        , "category"     .= PackageInfo.pkgCategory pkg
        , "dependencies" .= PackageInfo.pkgDependencies pkg
        , "maintainer"   .= PackageInfo.pkgMaintainer pkg
        , "name"         .= PackageInfo.pkgName pkg
        , "synopsis"     .= PackageInfo.pkgSynopsis pkg
        , "version"      .= PackageInfo.pkgVersion pkg
        ]

    index =
      Json.object
        [ "type"         .= ("package" :: String)
        , "indexed"      .= I.fmtTime "%FT%X" now
        , "description"  .= PackageInfo.pkgDescription pkg
        , "author"       .= PackageInfo.pkgAuthor pkg
        , "category"     .= PackageInfo.pkgCategory pkg
        , "dependencies" .= T.intercalate " " (PackageInfo.pkgDependencies pkg)
        , "name"         .= PackageInfo.pkgName pkg
        , "synopsis"     .= PackageInfo.pkgSynopsis pkg
        ]

    document =
      Json.object
        [ "description" .= description
        , "index"       .= index
        , "uri"         .= uri
        ]
  in
    Json.object
      [ "cmd"      .= ("insert" :: String)
      , "document" .= document
      ]


-- | Build a command for deleting a number of packages
-- from the Hunt index.
--
-- Here is an example of how that would look like for
-- the hayoo-indexer package.
--
-- > {
-- >   "cmd": "delete-by-query",
-- >   "query": {
-- >     "op": "and",
-- >     "type": "seq",
-- >     "args": [{
-- >       "type": "context",
-- >       "contexts": ["type"],
-- >       "query": {
-- >         "op": "case",
-- >         "type": "fullword",
-- >         "word": "package"
-- >       }
-- >     }, {
-- >       "type": "context",
-- >       "contexts": ["name"],
-- >       "query": {
-- >         "args": [{
-- >           "op": "case",
-- >           "type": "fullword",
-- >           "word": "hayoo-indexer"
-- >         }],
-- >         "op": "or",
-- >         "type": "seq"
-- >       }
-- >     }]
-- >   }
-- > }
delete :: [PackageInfo.PackageInfo] -> Json.Value
delete infos =
  let
    pkgNames =
      fmap PackageInfo.pkgName infos

    package =
      Json.object
        [ "type"     .= ("context" :: String)
        , "contexts" .= [ "type" :: String ]
        , "query"    .= I.fullWord "package"
        ]

    name =
      Json.object
        [ "type"     .= ("context" :: String)
        , "contexts" .= [ "name" :: String ]
        , "query"    .=
            Json.object
              [ "args" .= map I.fullWord pkgNames
              , "op"   .= ("or" :: String)
              , "type" .= ("seq" :: String)
              ]
        ]

    fullQuery =
      Json.object
        [ "args" .= [ package, name ]
        , "op"   .= ("and" :: String)
        , "type" .= ("seq" :: String)
        ]
  in
    Json.object
      [ "cmd"   .= ("delete-by-query" :: String)
      , "query" .= fullQuery
      ]
