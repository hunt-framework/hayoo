{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Hayoo.Templates where

-- import           Control.Lens

-- import           Data.Monoid (mconcat)

import           Data.String.Conversions (cs)
import           Data.Text.Lazy (Text)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T


import           Text.Blaze (preEscapedToMarkup, toValue)
import qualified Text.Blaze.Html.Renderer.String as Blaze (renderHtml)
import           Text.Blaze.Svg.Renderer.Text (renderSvg)
import           Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import qualified Text.Hamlet as Hamlet (HtmlUrl, hamlet, hamletFile, shamletFile)

-- import           Network.HTTP.Types (renderQuery, simpleQueryToQuery, Query)

import qualified Hunt.ClientInterface as H
-- import qualified Hunt.Server.Client as H

import           Hayoo.Common
import           Hayoo.Url

data Routes = Home | HayooJs | HayooCSS | Autocomplete | Examples | About



currentUrl :: Text -> Int -> Text
currentUrl q p = cs $ hayooQueryUrl p (cs q) -- TODO: this converts Text.Lazy to Text to Bytestring to Text and to Text.Lazy

contextUrl :: H.Query -> TS.Text
contextUrl q = hayooQueryUrl 0 $ printQuery q

render :: Routes -> [(TS.Text, TS.Text)] -> TS.Text
render Home _         = "/"
render HayooJs _      = "/hayoo.js"
render HayooCSS _     = "/hayoo.css"
render Autocomplete _ = "/autocomplete"
render Examples _     = "/examples"
render About _        = "/about"

renderTitle :: Text -> Text
renderTitle q
    | T.null q = "Hayoo! Haskell API Search"
    | otherwise = q `T.append` " - Hayoo!"

header :: Text -> Hamlet.HtmlUrl Routes
header q = [Hamlet.hamlet|
  <head>

    <title>#{renderTitle q}
    <script src="//ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js">
    <script src="//ajax.googleapis.com/ajax/libs/jqueryui/1.10.3/jquery-ui.min.js">
    <link href="//code.jquery.com/ui/1.10.3/themes/smoothness/jquery-ui.css" rel="stylesheet">

    <script src="//netdna.bootstrapcdn.com/bootstrap/3.0.3/js/bootstrap.min.js">
    <link href="//netdna.bootstrapcdn.com/bootstrap/3.0.3/css/bootstrap.min.css" rel="stylesheet">

    <link href=@{HayooCSS} rel="stylesheet">
    <script src=@{HayooJs}>

    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <link rel="search" title="Hayoo! Haskell API Search" href="opensearch.xml" type="application/opensearchdescription+xml"/>
    <link rel="icon" href="/favicon.ico">
    <script>
        var currentQuery = "#{q}"
|]
{--
navigation :: Text -> Hamlet.HtmlUrl Routes
navigation q = [Hamlet.hamlet|
<div .navbar .navbar-default .navbar-static-top role="navigation">

    <div .navbar-header .navbar-left>
        <a href=@{Home}>
            <img .logo src="/hayoo2.png" alt="Hayoo! logo" >
        <button type="button" .navbar-toggle data-toggle="collapse" data-target="#hayoo-navbar-collapse">
            <span .sr-only>Toggle navigation
            <span .icon-bar>
            <span .icon-bar>
            <span .icon-bar>

    <div .navbar-collapse .collapse #hayoo-navbar-collapse>
        <ul .nav .navbar-nav .navbar-left>
            <li .active>
                <form .navbar-form .navbar-left action="." method="get" id="search" role="search">
                    <div .form-group>
                        <input .form-control placeholder="Search" name="query" #hayoo type="text" autocomplete="off" accesskey="1" value="#{q}">
                    <input .btn .btn-primary #submit type="submit" value="Search">

|]
--}

navigation :: Text -> Hamlet.HtmlUrl Routes
navigation q = [Hamlet.hamlet|
<form action="." method="get" id="search" role="search">
  <div .row>
    <div .col-xs-3>
      <a href=@{Home}>
         <img .logo src="/hayoo2.png" alt="Hayoo! logo" >
    <div .col-xs-7>
      <input .form-control  placeholder="Search" name="query" #hayoo type="text" autocomplete="off" accesskey="1" value="#{q}">
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
        Github
|]

body :: Text -> Hamlet.HtmlUrl Routes -> T.Text
body q content = T.pack $ Blaze.renderHtml $ [Hamlet.hamlet|
$doctype 5
<html lang="en">
    ^{header q}
    <body>
      <div .container>
        <hr />
        ^{navigation q}
        <hr />
            ^{content}
        <hr />
      ^{footer}
|] render

packageUrl :: SearchResult -> Text
packageUrl r = cs $ hackagePackage $ resultPackage r

moduleUrl :: SearchResult -> Text
moduleUrl r = cs $ hackageModule $ resultUri r

ajax :: Hamlet.HtmlUrl Routes -> T.Text
ajax content = T.pack $ Blaze.renderHtml $ content render

-- ---------------------------------

renderBoxedResultHeading :: SearchResult -> Hamlet.HtmlUrl Routes
renderBoxedResultHeading r@(NonPackageResult {resultType=Method}) = [Hamlet.hamlet|
<div .panel-heading>
    <a href=#{resultUri r} .result-title>
        #{resultName r}
    :: #{resultSignature r}
    <span .label .label-default>
        Class Method
    ^{renderDropdown r}
|]

renderBoxedResultHeading r@(NonPackageResult {resultType=Function}) = [Hamlet.hamlet|
<div .panel-heading>
    <a href=#{resultUri r} .result-title>
        #{resultName r}
    :: #{resultSignature r}
    ^{renderDropdown r}
|]

renderBoxedResultHeading r@(NonPackageResult {}) = [Hamlet.hamlet|
<div .panel-heading>
    #{show $ resultType r}
    <a href=#{resultUri r} .result-title>
        #{resultName r}
    ^{renderDropdown r}
|]

renderBoxedResultHeading r@(PackageResult {}) = [Hamlet.hamlet|
<div .panel-heading>
    <a href=#{resultUri r} .result-title>
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
        <p .result-subtitle>
            <a href="#{packageUrl result}">
                #{resultPackage result}
            -
            $forall m <- resultModules result
                <a href="#{moduleUrl result}">
                    #{m}
                &nbsp;
        <div .description .more>
            #{toMarkup $ escapeScript resultDescription result}
|]

renderBoxedResult result@(PackageResult {}) = [Hamlet.hamlet|
<div .panel .panel-default>
    ^{renderBoxedResultHeading result}
    <div .panel-body>
        <div .description .more>
            #{escapeScript resultSynopsis result}
|]

renderBoxedResults :: H.LimitedResult SearchResult -> Hamlet.HtmlUrl Routes
--renderBoxedResults r = error $ show $ H.lrResult r
renderBoxedResults results = [Hamlet.hamlet|
<div #results>
    $if (H.lrCount results == 0)
      <div .alert .alert-danger>
        No results found
    $else
      $forall r <- H.lrResult results
        ^{renderBoxedResult r}
|]

resultContent :: H.LimitedResult SearchResult -> Hamlet.HtmlUrl Routes
resultContent results = [Hamlet.hamlet|
^{renderBoxedResults results}
$if ((H.lrCount results > 0) && (((H.lrMax results) * (1 + H.lrOffset results)) < (H.lrCount results)))
  <div .align-right>
    <button type="button" id="next-page-button" data-loading-text="Loading..." .btn .btn-primary>
        More...
$else

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
$if (H.lrCount lr > 0)
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
$else

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

about :: Hamlet.HtmlUrl Routes
about = $(Hamlet.hamletFile "about.html") -- use "make build" instead of "cabal build"!

examples :: Hamlet.HtmlUrl Routes
examples = $(Hamlet.hamletFile "examples.html") -- use "make build" instead of "cabal build"!

renderBadge :: TS.Text -> S.Markup
renderBadge version =
  S.docTypeSvg ! A.version "1.1" ! A.width (i totalWidth) ! A.height (i totalHeight) $ do
    S.defs $ do
      S.clippath ! A.id_ "clipPath" $
        S.rect ! A.x (i 0) ! A.y (i 0) ! A.width (i totalWidth) ! A.height (i totalHeight)
               ! A.rx (i cornerRadius) ! A.ry (i cornerRadius)
    S.g ! A.style "clip-path: url(#clipPath)" $ do
      S.g $ do
        S.path ! A.fill "#555" ! A.d leftPath
        S.path ! A.fill "#87BAEB" ! A.d rightPath
      S.g ! A.fill "#fff" ! A.textAnchor "middle" ! A.fontSize "12" ! A.fontFamily "sans-serif" $ do
        leftText   "Hayoo!"
        rightText  ('v' : TS.unpack version)
  where
  i' :: Int -> String
  i' = show

  i = toValue . i'

  totalWidth = 110
  totalHeight = 20
  partitionX = 50
  partitionY = totalWidth - partitionX
  cornerRadius = 3

  leftPath = toValue $
    concat ["M0 0 h", i' partitionX, " v", i' totalHeight, " H0 z"]

  rightPath = toValue $
    concat ["M", i' partitionX, " 0 h", i' partitionY, " v", i' totalHeight, " H", i' partitionX, " z"]

  textAt x y str =
    S.text_ ! A.x (i x) ! A.y (i y) ! A.dy ".3em" $ S.text (TS.pack str)

  leftText  = textAt (div partitionX 2)                               (div totalHeight 2)
  rightText = textAt (partitionX + (div (totalWidth - partitionX) 2)) (div totalHeight 2)
