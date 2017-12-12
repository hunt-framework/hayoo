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


anyZeroOrMany :: Hunt.RegEx
anyZeroOrMany =
  ".*"


exceptSpaceZeroOrMany :: Hunt.RegEx
exceptSpaceZeroOrMany =
  "[^ ]*"


wordSchema :: Hunt.ContextSchema
wordSchema =
  Hunt.mkSchema
    |> Hunt.setCxRegEx "\\w*"
    |> Hunt.setCxWeight 1.0



-- SIMPLIFIED HUNT API
--
-- Admittedly, this is not really necessary and
-- only adds some aliases for functions imported
-- from the Hunt.ClientInterface. However, those
-- functions are annoyingly hard to type and obscure
-- the essence of their functionality.


regex :: Hunt.RegEx -> Hunt.ContextSchema -> Hunt.ContextSchema
regex =
  Hunt.setCxRegEx


weight :: Float -> Hunt.ContextSchema -> Hunt.ContextSchema
weight =
  Hunt.setCxWeight


noDefault :: Hunt.ContextSchema -> Hunt.ContextSchema
noDefault =
  Hunt.setCxNoDefault


date :: Hunt.ContextSchema -> Hunt.ContextSchema
date =
  Hunt.setCxDate



-- ALL CONTEXTS AND THEIR SCHEMAS


contexts :: [ (Hunt.Context, Hunt.ContextSchema) ]
contexts =
  [ ( "author"
    , wordSchema
    )

  , ( "category"
    , wordSchema
        |> noDefault
    )

  , ( "dependencies"
    , wordSchema
        |> regex exceptSpaceZeroOrMany
        |> noDefault
    )

  , ( "description"
    , wordSchema
        |> weight 0.3
    )

  , ( "hierarchy"
    , wordSchema
        |> weight 0.1
    )

  , ( "indexed"
    , wordSchema
        |> regex dateRegex
        |> noDefault
        |> date
    )

  , ( "maintainer"
    , wordSchema
        |> noDefault
    )

  , ( "module"
    , wordSchema
        |> weight 0.5
        |> regex anyZeroOrMany
    )

  , ( "name"
    , wordSchema
        |> weight 3.0
        |> regex anyZeroOrMany
    )

  , ( "package"
    , wordSchema
        |> regex anyZeroOrMany
    )

  , ( "partial"
    , wordSchema
        |> weight 0.2
        |> regex exceptSpaceZeroOrMany
    )

  , ( "source"
    , wordSchema
        |> weight 0.1
        |> regex anyZeroOrMany
        |> noDefault
    )

  , ( "synopsis"
    , wordSchema
        |> weight 0.8
    )

  , ( "type"
    , wordSchema
        |> weight 0.0
        |> noDefault
    )

  , ( "upload"
    , wordSchema
        |> regex dateRegex
        |> noDefault
        |> date
    )

  , ( "version"
    , wordSchema
        |> regex anyZeroOrMany
        |> noDefault
    )

  , ( "signature"
    , wordSchema
        |> regex "[^$\n]*"
        |> noDefault
    )

  , ( "subsig"
    , wordSchema
        |> weight 1.5        -- signature weight * 0.5
        |> regex "[^$\n]*"
        |> noDefault
    )
  ]
