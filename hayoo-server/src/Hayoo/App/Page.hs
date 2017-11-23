{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Hayoo.App.Page
  ( index
  , about
  , examples
  , viewSearchResults
  ) where


import           Data.Maybe                      (fromMaybe)
import           Data.Monoid                     ((<>))
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as LT
import qualified Hayoo.App.Error                 as Hayoo
import           Servant.Client                  (ServantError (..))
import           Text.Blaze                      (toValue)
import           Text.Blaze.Html                 (Html)
import qualified Text.Blaze.Html.Renderer.String as Blaze (renderHtml)
import           Text.Blaze.Svg11                ((!))
import qualified Text.Blaze.Svg11                as S
import qualified Text.Blaze.Svg11.Attributes     as A
import qualified Text.Hamlet                     as Hamlet (HtmlUrl, hamlet,
                                                            hamletFile)

import           Hayoo.App.Types                 (ResultType (..),
                                                  SearchResult (..),
                                                  contextQueries,
                                                  contextQueryName,
                                                  contextQueryToQuery)
import           Hayoo.Internal.Helpers          ((|>))
import           Hayoo.Url
import qualified Hunt.ClientInterface            as H



-- INDEX


index :: Maybe LT.Text -> Maybe (Either Hayoo.Error (H.LimitedResult SearchResult)) -> Html
index maybeQuery result = page maybeQuery $
  case result of
    Nothing -> [Hamlet.hamlet|
<div .jumbotron>
  <h1>
      Hayoo! - Haskell API Search
  <p>
      Search #
      <a href="http://hackage.haskell.org/">Hackage
      \ by function, signature or package.
  <br />
  <p>
    <span class="glyphicon glyphicon-hand-right icon-span">
    How to use Hayoo!
    <a href="@{Examples}" >
      by Example
  <p>
    <span class="glyphicon glyphicon-hand-right icon-span">
    About
    <a href="@{About}" >
      Hayoo!
|]

    Just (Left err) ->
      viewError err

    Just (Right result) ->
      viewLimitedResult result



-- ABOUT


about :: Html
about =
  page Nothing $(Hamlet.hamletFile "about.html") -- use "make build" instead of "cabal build"!



-- EXAMPLES


examples :: Html
examples =
  page Nothing $(Hamlet.hamletFile "examples.html") -- use "make build" instead of "cabal build"!



-- ROUTES


data Routes
  = Home
  | Autocomplete
  | Examples
  | About


routesToUrl :: Routes -> [(T.Text, T.Text)] -> T.Text
routesToUrl routes _ =
  case routes of
    Home ->
      "/"

    Autocomplete ->
      "/autocomplete"

    Examples ->
      "/examples"

    About ->
      "/about"



-- URL HELPERS


currentUrl :: LT.Text -> Int -> LT.Text
currentUrl query p =
  query
    |> LT.toStrict
    |> hayooQueryUrl p
    |> LT.fromStrict


contextUrl :: H.Query -> T.Text
contextUrl query =
  query
    |> H.printQuery
    |> hayooQueryUrl 0


packageUrl :: SearchResult -> LT.Text
packageUrl result =
  result
    |> resultPackage
    |> hackagePackage
    |> LT.fromStrict


moduleUrl :: SearchResult -> LT.Text
moduleUrl result =
  result
    |> resultUri
    |> hackageModule
    |> LT.fromStrict



-- PAGE BUILDING BLOCKS


page :: Maybe LT.Text -> Hamlet.HtmlUrl Routes -> Html
page maybeQuery content = [Hamlet.hamlet|
$doctype 5
<html lang="en">
    ^{header maybeQuery}
    <body>
      <div .container>
        <hr />
        ^{navigation maybeQuery}
        <hr />
            ^{content}
        <hr />
      ^{footer}
|] routesToUrl


renderTitle :: Maybe LT.Text -> LT.Text
renderTitle maybeQuery =
  case maybeQuery of
    Nothing ->
      "Hayoo! Haskell API Search"

    Just query ->
      query <> " - Hayoo!"


header :: Maybe LT.Text -> Hamlet.HtmlUrl Routes
header query = [Hamlet.hamlet|
  <head>

    <title>#{renderTitle query}
    <script src="//ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js">
    <script src="//ajax.googleapis.com/ajax/libs/jqueryui/1.10.3/jquery-ui.min.js">
    <link href="//code.jquery.com/ui/1.10.3/themes/smoothness/jquery-ui.css" rel="stylesheet">

    <script src="//netdna.bootstrapcdn.com/bootstrap/3.0.3/js/bootstrap.min.js">
    <link href="//netdna.bootstrapcdn.com/bootstrap/3.0.3/css/bootstrap.min.css" rel="stylesheet">

    <link href="/static/data/hayoo.css" rel="stylesheet">
    <script src="/static/data/api.js">
    <script src="/static/data/hayoo.js">

    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <link rel="search" title="Hayoo! Haskell API Search" href="/static/data/opensearch.xml" type="application/opensearchdescription+xml"/>
    <link rel="icon" href="/static/data/favicon.ico">
    <script>
        var currentQuery = "#{fromMaybe "" query}"
|]


navigation :: Maybe LT.Text -> Hamlet.HtmlUrl Routes
navigation query = [Hamlet.hamlet|
<form action="." method="GET" id="search" role="search">
  <div .row>
    <div .col-xs-3>
      <a href=@{Home}>
         <img .logo src="/static/data/hayoo2.png" alt="Hayoo! logo" >
    <div .col-xs-7>
      <input .form-control  placeholder="Search" name="query" #hayoo type="text" autocomplete="off" accesskey="1" value="#{fromMaybe "" query}">
    <div .col-xs-2>
      <input .btn .btn-primary #submit type="submit" value="Search">
|]


footer :: Hamlet.HtmlUrl Routes
footer = [Hamlet.hamlet|
<footer id="footer">
    <a href=@{Home}>
      Hayoo 2.0.1 Frontend
    &copy; 2014 Sebastian Philipp
    |
    <a href=@{Examples}>Examples
    |
    <a href=@{About} >About
    <br />
      Powered by
      <a href="https://github.com/hunt-framework/hunt">
        Hunt
      |
      <a href="https://github.com/hunt-framework/hayoo">
        GitHub
|]



-- SEARCH RESULTS


viewSearchResults :: Either Hayoo.Error (H.LimitedResult SearchResult) -> Html
viewSearchResults result =
  case result of
    Left err ->
      viewError err routesToUrl

    Right searchResults ->
      viewLimitedResult searchResults routesToUrl



viewLimitedResult :: H.LimitedResult SearchResult -> Hamlet.HtmlUrl Routes
viewLimitedResult result = [Hamlet.hamlet|
<div #results>
    $if (H.lrCount result == 0)
      <div .alert .alert-danger>
        No results found
    $else
      $forall r <- H.lrResult result
        ^{viewSearchResult r}

^{viewPagination result}
|]


viewSearchResult :: SearchResult -> Hamlet.HtmlUrl Routes
viewSearchResult result =
  case result of
    NonPackageResult {} -> [Hamlet.hamlet|
<div .panel .panel-default>
    ^{viewSearchResultHeader result}
    <div .panel-body>
        <p .result-subtitle>
            <a href="#{packageUrl result}">
                #{resultPackage result}
            -
            $forall m <- resultModules result
                <a href="#{moduleUrl result}">
                    #{m}
                &nbsp;
        <div .description .more>
            #{S.preEscapedToMarkup $ escapeScript resultDescription result}
|]

    PackageResult {} -> [Hamlet.hamlet|
<div .panel .panel-default>
    ^{viewSearchResultHeader result}
    <div .panel-body>
        <div .description .more>
            #{escapeScript resultSynopsis result}
|]


viewSearchResultHeader :: SearchResult -> Hamlet.HtmlUrl Routes
viewSearchResultHeader result =
  case result of
    NonPackageResult {resultType=Method} -> [Hamlet.hamlet|
<div .panel-heading>
    <a href=#{resultUri result} .result-title>
        #{resultName result}
    :: #{resultSignature result}
    <span .label .label-default>
        Class Method
    ^{renderDropdown result}
|]

    NonPackageResult {resultType=Function} -> [Hamlet.hamlet|
<div .panel-heading>
    <a href=#{resultUri result} .result-title>
        #{resultName result}
    :: #{resultSignature result}
    ^{renderDropdown result}
|]

    NonPackageResult {} -> [Hamlet.hamlet|
<div .panel-heading>
    #{show $ resultType result}
    <a href=#{resultUri result} .result-title>
        #{resultName result}
    ^{renderDropdown result}
|]

    PackageResult {} -> [Hamlet.hamlet|
<div .panel-heading>
    <a href=#{resultUri result} .result-title>
        #{resultName result}
    <span .label .label-default>
        Package
    ^{renderDropdown result}
|]


viewPagination :: H.LimitedResult SearchResult -> Hamlet.HtmlUrl Routes
viewPagination results = [Hamlet.hamlet|
$if ((H.lrCount results > 0) && (((H.lrMax results) * (1 + H.lrOffset results)) < (H.lrCount results)))
  <div .align-right>
    <button type="button" id="next-page-button" data-loading-text="Loading..." .btn .btn-primary>
        More...
$else

|]


-- ERRORS


viewError :: Hayoo.Error -> Hamlet.HtmlUrl Routes
viewError hayooError =
  case hayooError of
    Hayoo.HuntClientError _ -> [Hamlet.hamlet|
<div .alert .alert-danger>
    <strong>
      It seems there is a problem with the connection or the result of
      the backing Hunt server. We are sorry, but it seems we are currently
      not able to deliver any search results to you. Still, feel free to
      take a look at the <a href=@{Examples}>examples</a> or what Hayoo! is
      all <a href=@{About}>about</a>.
|]

    Hayoo.InvalidCmdResult servantError -> [Hamlet.hamlet|
<div .alert .alert-danger>
    <strong>
      It seems there is a problem with the connection or the result of
      the backing Hunt server. We are sorry, but it seems we are currently
      not able to deliver any search results to you. Still, feel free to
      take a look at the <a href=@{Examples}>examples</a> or what Hayoo! is
      all <a href=@{About}>about</a>.
|]






-- HELPERS


escapeScript :: (SearchResult -> T.Text) -> SearchResult -> T.Text
escapeScript f sr =   T.replace "<script"  "&lt;script"
                    $ T.replace "</script" "&lt;/script"
                    $ f sr


renderDropdown :: SearchResult -> Hamlet.HtmlUrl Routes
renderDropdown r = renderDropdown' qs'
    where
    qs' = contextQueries r
    source = if Package == resultType r then
            ""
        else
            hackageSource (resultPackage r) $ resultSource r
    renderDropdown' [] = [Hamlet.hamlet|
|]
    renderDropdown' qs = [Hamlet.hamlet|
<div .pull-right .dropdown .text-primary id="fat-menu">
    <a href="#" id="drop3" role="button" .dropdown-toggle data-toggle="dropdown">
        More<b class="caret"></b>
        <ul .dropdown-menu role="menu" aria-labelledby="drop3">
            $forall (n, u) <- namedQueries
                <li role="presentation">
                    <a role="menuitem" tabindex="-1" href="#{u}">
                        #{n}
            $if not $ T.null source
                <li role="presentation" .divider>
                <li role="presentation">
                    <a role="menuitem" tabindex="-1" href="#{source}">
                        Source

|]
        where
        namedQueries = map (\q -> (contextQueryName q, contextUrl $ contextQueryToQuery q r)) qs
