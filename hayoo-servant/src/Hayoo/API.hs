{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Hayoo.API
  ( hayooAPI
  , HayooAPI
  ) where


import qualified Data.Text            as T
import           Hunt.ClientInterface (LimitedResult)
import           Servant
import           System.Metrics.Json  (Sample)
import           Types                (SearchResult)


-- API

hayooAPI :: Proxy HayooAPI
hayooAPI = Proxy


type HayooAPI = SearchAPI
           :<|> AutocompleteAPI
           :<|> MetricsAPI
           :<|> PublicFilesAPI


type AutocompleteAPI =
        "autocomplete"
        :> Capture "query" T.Text
        :> Get '[JSON] [T.Text]


type SearchAPI =
          "json" -- Legacy
          :> Capture "query" T.Text
          :> Get '[JSON] (LimitedResult SearchResult)
          "search"
          :> Capture "query" T.Text
          :> Get '[JSON] (LimitedResult SearchResult)


type MetricsAPI =
        "stats"   :> Get '[JSON] Sample
        "metrics" :> Get '[JSON] Sample


type PublicFilesAPI = Raw

