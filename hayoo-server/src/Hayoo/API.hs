{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Hayoo.API
  ( -- * API
    HayooAPI
  , RestAPI
  , HtmlAPI
  , SearchAPI
  , AutocompleteAPI
  , MetricsAPI

    -- * Proxy
  , hayooAPI
  , restAPI
  , htmlAPI
  ) where


import qualified Data.Text            as T
import           Hayoo.App.Types      (SearchResult)
import           Hunt.ClientInterface (LimitedResult)
import           Servant
import           Servant.HTML.Blaze
import           System.Metrics.Json  (Sample)
import           Text.Blaze.Html5     (Html)



-- PROXY


hayooAPI :: Proxy HayooAPI
hayooAPI =
  Proxy


restAPI :: Proxy RestAPI
restAPI =
  Proxy


htmlAPI :: Proxy HtmlAPI
htmlAPI =
  Proxy



-- API


type HayooAPI = RestAPI
           :<|> HtmlAPI
           :<|> "static" :> Raw


type RestAPI = SearchAPI
          :<|> AutocompleteAPI
          :<|> MetricsAPI


type SearchAPI =
          "json" -- Legacy
          :> QueryParam "query" T.Text
          :> QueryParam "page" Int
          :> Get '[JSON] (LimitedResult SearchResult)
     :<|> "search"
          :> Capture "query" T.Text
          :> QueryParam "page" Int
          :> Get '[JSON] (LimitedResult SearchResult)


type AutocompleteAPI =
        "autocomplete"
        :> Capture "query" T.Text
        :> Get '[JSON] [T.Text]


type MetricsAPI =
          "stats"   :> Get '[JSON] Sample
     :<|> "metrics" :> Get '[JSON] Sample


type HtmlAPI =
      "ajax"
      :> Capture "page" Int
      :> QueryParam "query" T.Text
      :> Get '[HTML] Html
 :<|> "about"
      :> Get '[HTML] Html
 :<|> "examples"
      :> Get '[HTML] Html
 :<|> QueryParam "query" T.Text
      :> Get '[HTML] Html
