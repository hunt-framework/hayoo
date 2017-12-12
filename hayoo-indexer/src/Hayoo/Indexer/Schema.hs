{-# LANGUAGE OverloadedStrings #-}
module Hayoo.Indexer.Schema
  ( insert
  , delete
  ) where


import qualified Data.Aeson             as Json
import qualified Data.String            as String
import           Hayoo.Indexer.Internal ((|>))
import qualified Hunt.ClientInterface   as Hunt



-- SCHEMA FOR HAYOO DATA


-- | Create a @Value@, describing all contexts and
-- their schemas, necessary for Hayoo!
--
-- The following contexts will be created:
--
--    * author
--    * category
--    * dependencies
--    * description
--    * hierarchy
--    * homepage
--    * indexed
--    * maintainer
--    * module
--    * name
--    * package
--    * partial
--    * signature
--    * source
--    * subsig
--    * synopsis
--    * type
--    * upload
--    * version
--
-- Here is an example of the resulting JSON:
--
-- > [
-- >   {
-- >     "cmd": "insert-context",
-- >     "context": "author",
-- >     "schema": {
-- >       "regexp": "\\w*",
-- >       "type": "text",
-- >     }
-- >   },
-- >   ...
-- > ]
insert :: Json.Value
insert =
  contexts
    |> fmap (uncurry Hunt.cmdInsertContext)
    |> Hunt.cmdSequence
    |> Json.toJSON


-- | Create a @Value@ describing the removal of all
-- contexts from Hunt.
--
-- All contexts documented in @insert@ will be removed.
--
-- Here is an example of the resulting JSON:
--
-- > [
-- >   {
-- >     "cmd": "delete-context",
-- >     "context": "author"
-- >   },
-- >   ...
-- > ]
delete :: Json.Value
delete =
  contexts
    |> fmap fst
    |> fmap Hunt.cmdDeleteContext
    |> Json.toJSON



-- PRIVATE REGEX AND SCHEMA HELPERS


dateRegex :: Hunt.RegEx
dateRegex =
  let
    year =
      "[0-9]{4}" -- year

    d2 =
      "[0-9]{2}"

    ms =
      "-"
  in
    String.fromString $
      mconcat [ year, "(", ms, d2, "(", ms, d2 , "(T", d2, ":", d2, ":", d2, ")?)?)?" ]


anyLetter :: Hunt.RegEx
anyLetter =
  ".*"


anyExceptSpace :: Hunt.RegEx
anyExceptSpace =
  "[^ ]*"


wordSchema :: Hunt.ContextSchema
wordSchema =
  Hunt.mkSchema
    |> Hunt.setCxRegEx "\\w*"
    |> Hunt.setCxWeight 1.0



-- ALL CONTEXTS AND THEIR SCHEMAS


contexts :: [ (Hunt.Context, Hunt.ContextSchema) ]
contexts =
  [ ( "author"
    , wordSchema
    )

  , ( "category"
    , wordSchema
        |> Hunt.setCxNoDefault
    )

  , ( "dependencies"
    , wordSchema
        |> Hunt.setCxRegEx anyExceptSpace
        |> Hunt.setCxNoDefault
    )

  , ( "description"
    , wordSchema
        |> Hunt.setCxWeight 0.3
    )

  , ( "hierarchy"
    , wordSchema
        |> Hunt.setCxWeight 0.1
    )

  , ( "indexed"
    , wordSchema
        |> Hunt.setCxRegEx dateRegex
        |> Hunt.setCxNoDefault
        |> Hunt.setCxDate
    )

  , ( "maintainer"
    , wordSchema
        |> Hunt.setCxNoDefault
    )

  , ( "module"
    , wordSchema
        |> Hunt.setCxWeight 0.5
        |> Hunt.setCxRegEx anyLetter
    )

  , ( "name"
    , wordSchema
        |> Hunt.setCxWeight 3.0
        |> Hunt.setCxRegEx anyLetter
    )

  , ( "package"
    , wordSchema
        |> Hunt.setCxRegEx anyLetter
    )

  , ( "partial"
    , wordSchema
        |> Hunt.setCxWeight 0.2
        |> Hunt.setCxRegEx anyExceptSpace
    )

  , ( "source"
    , wordSchema
        |> Hunt.setCxWeight 0.1
        |> Hunt.setCxRegEx anyLetter
        |> Hunt.setCxNoDefault
    )

  , ( "synopsis"
    , wordSchema
        |> Hunt.setCxWeight 0.8
    )

  , ( "type"
    , wordSchema
        |> Hunt.setCxWeight 0.0
        |> Hunt.setCxNoDefault
    )

  , ( "upload"
    , wordSchema
        |> Hunt.setCxRegEx dateRegex
        |> Hunt.setCxNoDefault
        |> Hunt.setCxDate
    )

  , ( "version"
    , wordSchema
        |> Hunt.setCxRegEx anyLetter
        |> Hunt.setCxNoDefault
    )

  , ( "signature"
    , wordSchema
        |> Hunt.setCxRegEx "[^$\n]*"
        |> Hunt.setCxNoDefault
    )

  , ( "subsig"
    , wordSchema
        |> Hunt.setCxWeight 1.5        -- signature weight * 0.5
        |> Hunt.setCxRegEx "[^$\n]*"
        |> Hunt.setCxNoDefault
    )
  ]
