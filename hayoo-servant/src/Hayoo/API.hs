{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Hayoo.API
  ( -- * API
    HayooAPI
  , RestAPI
  , SearchAPI
  , AutocompleteAPI
  , MetricsAPI

    -- * Proxy
  , hayooAPI
  ) where


import qualified Data.Text            as T
import           Hayoo.Types          (SearchResult)
import           Hunt.ClientInterface (LimitedResult)
import           Servant
import           System.Metrics.Json  (Sample)


-- API

hayooAPI :: Proxy HayooAPI
hayooAPI = Proxy


type HayooAPI = RestAPI
           :<|> Raw


type RestAPI = SearchAPI
          :<|> AutocompleteAPI
          :<|> MetricsAPI


type SearchAPI =
          "json" -- Legacy
          :> Capture "query" T.Text
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


