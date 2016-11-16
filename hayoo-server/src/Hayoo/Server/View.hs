{-# LANGUAGE OverloadedStrings #-}
module Hayoo.Server.View
  ( searchPage
  ) where

import qualified Data.Text                   as T
import           Prelude                     hiding (head)
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A


-- PAGES

searchPage :: T.Text -> H.Html
searchPage query = html "Hayoo! Search engine" $ do
  search query
  


-- PAGE CONTENT

search :: T.Text -> H.Html
search query =
  H.div $ do
    searchBox query


-- COMPONENTS

searchBox :: T.Text -> H.Html
searchBox value =
  H.form ! A.method "GET" ! H.customAttribute "role" "search" ! A.class_ "search" $ do
    searchInput value
    H.button ! A.type_ "submit" $ "Search"


searchInput :: T.Text -> H.Html
searchInput query =
  H.input
    ! A.type_ "text"
    ! A.placeholder "Search..."
    ! A.accesskey "1"
    ! A.name "query"
    ! A.value (H.toValue query)
  

-- SHARED

html :: T.Text -> H.Html -> H.Html
html title body = H.docTypeHtml $ do
  head title
  H.body body


head :: T.Text -> H.Html
head title = H.head $ do
  H.title (H.toHtml title)
  H.link ! A.rel "favicon"    ! A.href "favicon.ico"
  H.link ! A.rel "stylesheet" ! A.href "hayoo.css"
  H.meta ! A.name "viewport"  ! A.content "width=device-width, initial-scale=1.0"
  opensearchLink



-- HELPERS

opensearchLink :: H.Html
opensearchLink =
  H.link
    ! A.href  "opensearch.xml"
    ! A.rel   "search"
    ! A.title "Hayoo! Haskell API Search"
    ! A.type_ "application/opensearchdescription"

