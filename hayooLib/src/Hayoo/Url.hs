{-# LANGUAGE OverloadedStrings #-}

module Hayoo.Url
    (
      queryForDocument
    , hayooQueryUrl
    , hayooUrl
    , urlForDocument
    , hackageSource
    , hackageModule
    , hackagePackage
    )
where


import           Data.String.Conversions (cs, (<>))
import           Data.Text               (Text)

import qualified Hunt.ClientInterface    as H (Query, printQuery, qAnds,
                                               qContext, qOrs, setBoost)

import           Network.HTTP.Types      (renderQuery, simpleQueryToQuery)

import           Network.URI             (URI(..), parseURI)


-- | generates a rendered 'Query' by a @package@, a @module@ and a @function or type@.

queryForDocument :: Text -> Text -> Text -> H.Query
queryForDocument p m f
    = H.qOrs [H.setBoost 100 q1, H.setBoost 10 q2, q3]
    where
      q1 = H.qAnds ["name" `H.qContext` f, "package" `H.qContext` p, "module" `H.qContext` m]
      q2 = H.qAnds ["name" `H.qContext` f, "package" `H.qContext` p]
      q3 = H.qAnds ["name" `H.qContext` f]


-- | gernerate a url to a @page@ and a 'H.Query'.

hayooQueryUrl :: Int -> Text -> Text
hayooQueryUrl 0 q = hayooUrl [("query", q)]
hayooQueryUrl p q = hayooUrl [("query", q), ("page", cs $ show p)]

-- | generte a url with a list or parameters

hayooUrl :: [(Text, Text)] -> Text
hayooUrl q
    = ("/") <> (cs $ renderQuery True $ simpleQueryToQuery q')
    where
      -- convert [(Text, Text)] -> [(ByteString, ByteString)]
      q' = map (\(x, y) -> (cs x, cs y)) q

-- | gernerate a url to a 'Query' by a @package@, a @module@ and a @function or type@.

urlForDocument :: Text -> Text -> Text -> Text
urlForDocument package modul func
    = hayooQueryUrl 0 $ H.printQuery $ queryForDocument package modul func

hackageSource :: Text -> Text -> Text
hackageSource p sub
    = "http://hackage.haskell.org/package/" <> p <> "/docs/" <> sub


hackageModule :: Text -> Text
hackageModule url
    = cs $ show modUrl
      where
      empty = URI "" Nothing "" "" ""
      functionUrl = maybe empty id $ parseURI $ cs url
      modUrl = functionUrl {uriFragment = ""}

hackagePackage :: Text -> Text
hackagePackage pac
    = "http://hackage.haskell.org/package/" <> pac
