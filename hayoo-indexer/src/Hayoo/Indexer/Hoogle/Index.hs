{-# LANGUAGE OverloadedStrings #-}
module Hayoo.Indexer.Hoogle.Index
  ( insert
  , delete
  ) where


import           Data.Aeson             as Json
import qualified Data.Char              as Char
import qualified Data.Maybe             as Maybe
import           Data.Semigroup         ((<>))
import qualified Data.Text              as T
import qualified Data.Time              as Time
import qualified Hayoo.Core.DeclInfo    as DeclInfo
import           Hayoo.Indexer.Internal ((|>))
import qualified Hayoo.Indexer.Internal as I



-- INDEX DECL INFO


-- | Build a command for inserting a @DeclInfo@ into
-- a Hunt index.
--
-- Here is an example of how that would look like for
-- the `decode` function in the `Data.Aeson` package.
--
-- > {
-- >   "cmd": "insert",
-- >   "document": {
-- >     "uri": "http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#v:decode",
-- >     "description": {
-- >       "indexed": "Mon Jan 01 10:00:00 2017",
-- >       "description": "An efficient ...",
-- >       "package": "aeson",
-- >       "module": "Data.Aeson",
-- >       "name": "decode",
-- >       "type": "function",
-- >       "source": "",
-- >     },
-- >     "index": {
-- >       "package": "aeson",
-- >       "indexed": "2017-01-01T10:00:00",
-- >       "description": "An efficient ...",
-- >       "module": "Data.Aeson",
-- >       "name": "decode",
-- >       "type": "function",
-- >       "hierarchy": "Data Aeson",
-- >       "signature": "(FromJSON a) => ByteString -> Maybe a",
-- >       "subsig": ""
-- >     }
-- >   }
-- > }
insert :: Time.UTCTime -> DeclInfo.DeclInfo -> Json.Value
insert now info =
  let
    declDescription =
      info
        |> DeclInfo.description
        |> fmap (\value -> [ ("description" :: T.Text) .= value ])
        |> Maybe.fromMaybe []

    type_ =
      info
        |> DeclInfo.declType
        |> show
        |> T.pack
        |> firstToLower

    signature =
      case DeclInfo.signature info of
        Nothing ->
          []

        Just sig ->
          [ "signature" .= sig
          , "subsig"    .= sig -- TODO: need subsignatures here
          ]

    description =
      Json.object $
        [ "indexed" .= I.fmtTime "%c" now
        , "package" .= DeclInfo.package info
        , "module"  .= DeclInfo.moduleName info
        , "type"    .= type_
        , "source"  .= Maybe.fromMaybe "" (DeclInfo.sourceURI info)
        ] <> declDescription

    index =
      Json.object $
        [ "package"   .= (DeclInfo.package info)
        , "module"    .= (DeclInfo.moduleName info)
        , "name"      .= (DeclInfo.name info)
        , "type"      .= type_
        , "hierarchy" .= T.replace "." " " (DeclInfo.moduleName info)
        ] <> declDescription <> signature

    document =
      Json.object
        [ "description" .= description
        , "index"       .= index
        , "uri"         .= DeclInfo.docURI info
        ]
  in
    Json.object
      [ "cmd"      .= ("insert" :: String)
      , "document" .= document
      ]


-- | Build a statement for deleting packages
--
-- Here is an example for such a query for the
-- package `aeson`.
--
-- > {
-- >   "cmd": "insert",
-- >   "query": {
-- >     "type": "context",
-- >     "contexts": [ "package" ],
-- >     "query": {
-- >       "op": "case",
-- >       "type": "fullword",
-- >       "word": "aeson",
-- >     }
-- >   }
-- > }
delete :: T.Text -> Json.Value
delete pkgName =
  Json.object
    [ "cmd"   .= ( "delete-by-query" :: String )
    , "query" .=
      Json.object
        [ "type"     .= ( "context" :: String )
        , "contexts" .= [ "package" :: String ]
        , "query"    .= I.fullWord pkgName
        ]
    ]



-- HELPERS


firstToLower :: T.Text -> T.Text
firstToLower input =
  case T.uncons input of
    Nothing ->
      input

    Just (first, rest) ->
      T.cons (Char.toLower first) rest
