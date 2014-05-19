{-# LANGUAGE OverloadedStrings #-}

module Hayoo.Url 
(
      queryForDocument
    , hayooQueryUrl
    , hayooUrl
    , urlForDocument
)
where


import           Data.String.Conversions (cs, (<>))
import           Data.Text (Text)
import qualified Hunt.ClientInterface as H (Query, printQuery,  qOrs, qAnds, withBoost, qContext)
import           Network.HTTP.Types (renderQuery, simpleQueryToQuery)

-- | generates a rendered 'Query' by a @package@, a @module@ and a @function or type@.
queryForDocument :: Text -> Text -> Text -> H.Query
queryForDocument p m f = H.qOrs [H.withBoost 100 q1, H.withBoost 10 q2, q3]
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
hayooUrl q = ("/") <> (cs $ renderQuery True $ simpleQueryToQuery q')
    where
    -- convert [(Text, Text)] -> [(ByteString, ByteString)]
    q' = map (\(x, y) -> (cs x, cs y)) q

-- | gernerate a url to a 'Query' by a @package@, a @module@ and a @function or type@. 
urlForDocument :: Text -> Text -> Text -> Text
urlForDocument package mod func = hayooQueryUrl 0 $ H.printQuery $ queryForDocument package mod func
