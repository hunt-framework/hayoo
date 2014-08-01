{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Hayoo.Templates where

import           Data.String.Conversions (cs)
import           Data.Text.Lazy (Text)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T


import qualified Text.Hamlet as Hamlet (HtmlUrl, hamlet, hamletFile)
import qualified Text.Blaze.Html.Renderer.String as Blaze (renderHtml)
import           Text.Blaze (preEscapedToMarkup)

import qualified Hunt.ClientInterface as H

import           Hayoo.Common
import           Hayoo.Url


data Routes = Home | HayooJs | HayooCSS | Autocomplete | Examples | About



currentUrl :: Text -> Int -> Text
currentUrl q p = cs $ hayooQueryUrl p (cs q) -- TODO: this converts Text.Lazy to Text to Bytestring to Text and to Text.Lazy

contextUrl :: H.Query -> TS.Text
contextUrl q = hayooQueryUrl 0 $ printQuery q

render :: Routes -> [(TS.Text, TS.Text)] -> TS.Text
render Home _ = "/"
render HayooJs _ = "/hayoo.js"
render HayooCSS _ = "/hayoo.css"
render Autocomplete _ = "/autocomplete"
render Examples _ = "/#/examples"
render About _ = "/#/about"



navigation :: Text -> Hamlet.HtmlUrl Routes
navigation query = $(Hamlet.hamletFile "navigation.html")

renderLayout :: Text -> Hamlet.HtmlUrl Routes -> T.Text
renderLayout query content = T.pack . Blaze.renderHtml $ $(Hamlet.hamletFile "index.html") render

results :: Hamlet.HtmlUrl Routes
results  = $(Hamlet.hamletFile "results.html")

about :: Hamlet.HtmlUrl Routes
about = $(Hamlet.hamletFile "about.html") -- use "make build" instead of "cabal build"!

examples :: Hamlet.HtmlUrl Routes
examples = $(Hamlet.hamletFile "examples.html") -- use "make build" instead of "cabal build"!

packageUrl :: SearchResult -> Text
packageUrl r = cs $ hackagePackage $ resultPackage r

moduleUrl :: SearchResult -> Text
moduleUrl r = cs $ hackageModule $ resultUri r

renderAjax :: Hamlet.HtmlUrl Routes -> T.Text
renderAjax content = T.pack $ Blaze.renderHtml $ content render

-- ---------------------------------

renderBoxedResultHeading :: SearchResult -> Hamlet.HtmlUrl Routes
renderBoxedResultHeading r@(NonPackageResult {resultType=Method}) = [Hamlet.hamlet|
<div .panel-heading>
    <a href=#{resultUri r}>
        #{resultName r}
    :: #{resultSignature r}
    <span .label .label-default>
        Class Method
    ^{renderDropdown r}
|]

renderBoxedResultHeading r@(NonPackageResult {resultType=Function}) = [Hamlet.hamlet|
<div .panel-heading>
    <a href=#{resultUri r}>
        #{resultName r}
    :: #{resultSignature r}
    ^{renderDropdown r}
|]

renderBoxedResultHeading r@(NonPackageResult {}) = [Hamlet.hamlet|
<div .panel-heading>
    #{show $ resultType r}
    <a href=#{resultUri r}>
        #{resultName r}
    ^{renderDropdown r}
|]

renderBoxedResultHeading r@(PackageResult {}) = [Hamlet.hamlet|
<div .panel-heading>
    <a href=#{resultUri r}>
        #{resultName r}
    <span .label .label-default>
        Package
    ^{renderDropdown r}
|]

renderBoxedResult :: SearchResult -> Hamlet.HtmlUrl Routes
renderBoxedResult result@(NonPackageResult {}) = [Hamlet.hamlet|
<div .panel .panel-default>
    ^{renderBoxedResultHeading result}
    <div .panel-body>
        <p>
            <a href="#{packageUrl result}">
                #{resultPackage result}
            -
            $forall m <- resultModules result
                <a href="#{moduleUrl result}">
                    #{m}
                &nbsp;
        <div .description .more>
            #{preEscapedToMarkup $ resultDescription result}
|]

renderBoxedResult result@(PackageResult {}) = [Hamlet.hamlet|
<div .panel .panel-default>
    ^{renderBoxedResultHeading result}
    <div .panel-body>
        <div .description .more>
            #{resultSynopsis result}
|]

renderBoxedResults :: H.LimitedResult SearchResult -> Hamlet.HtmlUrl Routes
--renderBoxedResults r = error $ show $ H.lrResult r
renderBoxedResults results = [Hamlet.hamlet|
<div #results>
    $forall r <- H.lrResult results
        ^{renderBoxedResult r}
|]

resultContent :: H.LimitedResult SearchResult -> Hamlet.HtmlUrl Routes
resultContent results = [Hamlet.hamlet|
^{renderBoxedResults results}
<div>
    <button type="button" id="next-page-button" data-loading-text="Loading..." .btn .btn-primary>
        Next Page
|]

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
            $if not $ TS.null source
                <li role="presentation" .divider>
                <li role="presentation">
                    <a role="menuitem" tabindex="-1" href="#{source}">
                        Source

|]
        where
        namedQueries = map (\q -> (contextQueryName q, contextUrl $ contextQueryToQuery q r)) qs





renderPagination :: Text -> H.LimitedResult a -> Hamlet.HtmlUrl Routes
renderPagination query' lr = [Hamlet.hamlet|
<ul .pagination>
  $if isFirstPage
      <li .disabled>
          <span>&laquo;
  $else
      <li>
          <a href="#{currentUrl query' leftArrowPage}">&laquo;
  $forall page <- pages
      $if page == currentPage
          <li .disabled>
              <span>#{page + 1}
      $else
          <li>
              <a href="#{currentUrl query' page}">#{page + 1}
  $if isLastPage
      <li .disabled>
          <span>&raquo;
  $else
      <li>
          <a href="#{currentUrl query' rightArrowPage}">&raquo;

|]
    where
        isFirstPage = currentPage == 0
        isLastPage = currentPage == lastPage
        page offset max' = offset `div` max'
        lastPage = page (H.lrCount lr) (H.lrMax lr)
        currentPage = page (H.lrOffset lr) (H.lrMax lr)

        firstPagerPage = (currentPage - 2) `max` 0
        lastPagerPage = ((currentPage + 2) `max` 5) `min` lastPage
        leftArrowPage = (firstPagerPage - 3) `max` 0
        rightArrowPage = (lastPagerPage + 3) `min` lastPage
        pages = [firstPagerPage .. lastPagerPage]


renderException :: HayooException -> Hamlet.HtmlUrl Routes
renderException (StringException e) =  [Hamlet.hamlet|
<div .alert .alert-danger>
    <strong>
        Internal Error:
    #{e}
|]

renderException (HuntClientException e) =  [Hamlet.hamlet|
<div .alert .alert-warning>
    <strong>
        Internal Error:
    #{show e}
|]

renderException (HttpException e) =  [Hamlet.hamlet|
<div .alert .alert-danger>
    <strong>
        Connection Error:
    #{show e}
|]

renderException (ParseError e) =  [Hamlet.hamlet|
<div .alert .alert-info>
    <strong>
        Parse Error:
    #{show e}
|]

renderException FileNotFound = [Hamlet.hamlet|
<div .alert .alert-info>
    <strong>
        404:
    File Not Found.
|]

mainPage :: Hamlet.HtmlUrl Routes
mainPage = [Hamlet.hamlet|
<div .jumbotron>
  <h1>
      Hayoo! - Haskell Api Search
  <p>
      Search for Packages, Functions and Signatures in
      <a href="http://hackage.haskell.org/">Hackage#
      .
|]




