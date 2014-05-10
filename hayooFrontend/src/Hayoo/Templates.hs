{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Hayoo.Templates where

-- import           Control.Applicative ((<$>))
import           Control.Lens

-- import           Data.Map (Map, toList)
import           Data.Monoid (mconcat)

import           Data.String.Conversions (cs, (<>), ConvertibleStrings)
import           Data.Text.Lazy (Text)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T

import qualified Hunt.Server.Client as Api

import qualified Text.Hamlet as Hamlet (HtmlUrl, hamlet)
import qualified Text.Blaze.Html.Renderer.String as Blaze (renderHtml)
import           Text.Blaze (preEscapedToMarkup)

import           Network.HTTP.Types (renderQuery, simpleQueryToQuery, Query)


import           Hayoo.Common

data Routes = Home | Simple | HayooJs | HayooCSS | Autocomplete | Examples | About

urlQ :: (ConvertibleStrings TS.Text b, ConvertibleStrings a TS.Text) => Routes -> a -> Int -> b
urlQ r q 0 = cs $ url r [("query", cs q)]
urlQ r q p = cs $ url r [("query", cs q), ("page", cs $ show p)]

urlQ0 :: (ConvertibleStrings TS.Text b, ConvertibleStrings a TS.Text) => a -> b
urlQ0 q = urlQ Home q 0

urlQ'L :: Text -> Int -> Text
urlQ'L = urlQ Home 

urlQ'L' :: Routes -> Text -> Int -> Text
urlQ'L' r = urlQ r 

urlQ'S :: TS.Text -> Int -> TS.Text
urlQ'S = urlQ Home 

url :: Routes -> [(TS.Text, TS.Text)] -> TS.Text
url r q = cs $ (cs $ render r []) <> (renderQuery True simpleQuery)
    where
    simpleQuery :: Query
    simpleQuery = simpleQueryToQuery $ over (mapped . both) cs $ q

render :: Routes -> [(TS.Text, TS.Text)] -> TS.Text
render Home _ = "/"
render Simple _ = "/simple/"
render HayooJs _ = "/hayoo.js"
render HayooCSS _ = "/hayoo.css"
render Autocomplete _ = "/autocomplete"
render Examples _ = "/examples"
render About _ = "/about"

renderTitle :: Text -> Text
renderTitle q
    | T.null q = "Hayoo! Haskell API Search"
    | otherwise = q `T.append` " - Hayoo!"

--header :: Blaze.Html
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
|]

navigation :: Text -> DisplayType -> Hamlet.HtmlUrl Routes
navigation q t = [Hamlet.hamlet|
<div .navbar .navbar-default .navbar-static-top role="navigation">

    <div .navbar-header .navbar-left>
        <a href=@{Home}>
            <img .logo src="//holumbus.fh-wedel.de/hayoo/hayoo.png" alt="Hayoo! logo" >
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
                    <input .btn .btn-default #submit type="submit" value="Search">

        <ul .nav .navbar-nav .navbar-right>
            <li .active>
                ^{otherResultType q t}
            <li>
                <a href=@{Examples}>Examples
            <li>
                <a href=@{About}>About
|]


footer :: Hamlet.HtmlUrl Routes
footer = [Hamlet.hamlet|
<footer>
    <a href=@{Home}> Hayoo Frontend
    &copy; 2014 Sebastian Philipp
|]

body :: Text -> DisplayType -> Hamlet.HtmlUrl Routes -> T.Text 
body q t content = T.pack $ Blaze.renderHtml $ [Hamlet.hamlet|
$doctype 5
<html lang="en">
    ^{header q}
    <body>
        ^{navigation q t}
        
        <div class="container">
            ^{content}
        
        ^{footer}
|] render

mainUri :: a -> a
mainUri m = m --snd $ head $ toList m

ajax :: Hamlet.HtmlUrl Routes -> T.Text 
ajax content = T.pack $ Blaze.renderHtml $ content render

otherResultType :: Text -> DisplayType -> Hamlet.HtmlUrl Routes
otherResultType q Boxed = [Hamlet.hamlet|
<a href="#{urlQ'L' Simple q 0}">
    Simple
|]

otherResultType q Grouped = [Hamlet.hamlet|
<a href="#{urlQ'L' Home q 0}">
    Grouped
|]

-- ---------------------------------

renderBoxedResultHeading :: SearchResult -> Hamlet.HtmlUrl Routes
renderBoxedResultHeading r@(NonPackageResult {resultType=Method}) = [Hamlet.hamlet|
<div .panel-heading>
    <a href=#{mainUri $ resultUri r}>
        #{resultName r}
    :: #{resultSignature r}
    <span .label .label-default>
        Class Method
|]

renderBoxedResultHeading r@(NonPackageResult {resultType=Function}) = [Hamlet.hamlet|
<div .panel-heading>
    <a href=#{mainUri $ resultUri r}>
        #{resultName r}
    :: #{resultSignature r}
|]

renderBoxedResultHeading r@(NonPackageResult {}) = [Hamlet.hamlet|
<div .panel-heading>
    #{show $ resultType r}
    <a href=#{mainUri $ resultUri r}>
        #{resultName r}
|]

renderBoxedResultHeading r@(PackageResult {}) = [Hamlet.hamlet|
<div .panel-heading>
    <a href=#{mainUri $ resultUri r}>
        #{resultName r}
    <span .label .label-default>
        Package
|]

renderBoxedResult :: SearchResult -> Hamlet.HtmlUrl Routes
renderBoxedResult result@(NonPackageResult {}) = [Hamlet.hamlet|
<div .panel .panel-default>
    ^{renderBoxedResultHeading result} 
    <div .panel-body>
        <p>
            #{resultPackage result} - #{resultModule result}
        <p .description .more>
            #{preEscapedToMarkup $ resultDescription result}
|]

renderBoxedResult result@(PackageResult {}) = [Hamlet.hamlet|
<div .panel .panel-default>
    ^{renderBoxedResultHeading result} 
    <div .panel-body>
        <p .description .more>
            #{resultSynopsis result}
|]

renderBoxedResults :: Api.LimitedResult SearchResult -> Hamlet.HtmlUrl Routes
--renderBoxedResults r = error $ show $ Api.lrResult r
renderBoxedResults results = [Hamlet.hamlet|
$forall r <- Api.lrResult results
    ^{renderBoxedResult r}
|]
-- -------------------------------------

renderResult :: SearchResult -> Hamlet.HtmlUrl Routes
renderResult r@(NonPackageResult {resultType=Method}) = [Hamlet.hamlet|
<p .resultLine>
    <a href=#{mainUri $ resultUri r}>
        #{resultName r}
    :: #{resultSignature r}
    <span .label .label-default>
        Class Method
<p .description .more>
    #{preEscapedToMarkup $ resultDescription r}
|]

renderResult r@(NonPackageResult {resultType=Function}) = [Hamlet.hamlet|
<p .resultLine>
    <a href=#{mainUri $ resultUri r}>
        #{resultName r}
    :: #{resultSignature r}
<p .description .more>
    #{preEscapedToMarkup $ resultDescription r}
|]

renderResult r@(NonPackageResult {}) = [Hamlet.hamlet|
<p .resultLine>
    #{show $ resultType r}
    <a href=#{mainUri $ resultUri r}>
        #{resultName r}
<p .description .more>
    #{preEscapedToMarkup $ resultDescription r}
|]

renderResult r@(PackageResult {}) = [Hamlet.hamlet|
<p .resultLine>
    package
    <a href=#{mainUri $ resultUri r}>
        #{resultName r}
|]


renderResults :: Api.LimitedResult SearchResult -> Hamlet.HtmlUrl Routes
renderResults results = [Hamlet.hamlet|
$forall r <- Api.lrResult results
    ^{renderResult r}
|]

breadcrump :: ModuleResult -> [(TS.Text, TS.Text)]
breadcrump (_, r:_) = [
        (urlQ0 ("package:" <> (resultPackage r)), resultPackage r),
        (urlQ0 (mconcat ["package:", resultPackage r, " module:", resultModule r]), resultModule r)
    ]
breadcrump (Just r, _) = [
        (urlQ0 ("package:" <> (resultPackage r)), resultPackage r),
        (urlQ0 (mconcat ["package:", resultPackage r, " module:", resultModule r]), resultModule r)
    ]
breadcrump (_, []) = error "breadcrump: empty"

moduleHtmlId :: ModuleResult -> TS.Text
moduleHtmlId (_, r:_) = resultPackage r <> "-" <> (TS.replace "." "-" $ resultModule r)
moduleHtmlId (Just r, _) = resultPackage r <> "-" <> (TS.replace "." "-" $ resultModule r)
moduleHtmlId (_, []) = error "moduleHtmlId: empty"

moduleHtmlQuery :: Text -> ModuleResult -> TS.Text
moduleHtmlQuery q (_, r:_) = resultPackage r <> "/" <> resultModule r <> "/" <> (cs q)
moduleHtmlQuery q (Just r, _) = resultPackage r <> "/" <> resultModule r <> "/" <> (cs q)
moduleHtmlQuery _ (_, []) = error "moduleHtmlQuery: empty"

renderModule :: Text -> ModuleResult -> Hamlet.HtmlUrl Routes
renderModule q m = [Hamlet.hamlet|
<div .panel-heading>
    <ol .breadcrumb>
        $forall (url, part) <- (breadcrump m)
            <li>
                <a href="#{url}">#{part}
<div .panel-body id="#{moduleHtmlId m}">
    $forall r <- (snd m)
        ^{renderResult r}
    <button type="button" href="#" .btn .btn-default .btn-xs onclick="fillModule('#{moduleHtmlId m}','#{moduleHtmlQuery q m}')">
        show more...
|]

renderPackage :: Text -> PackageResult ->  Hamlet.HtmlUrl Routes
renderPackage q (Just res, []) = [Hamlet.hamlet|
<div .panel .panel-default>
    <div .panel-heading>
        <ol .breadcrumb>
            <li>
                <a href="#{url'}">#{resultName res}
    <div .panel-body>
        #{resultSynopsis res}
        <!--<button type="button" href="#" .btn .btn-default .btn-xs onclick="fillPackage('#{moduleHtmlId m}','#{moduleHtmlQuery q m}')">
            show more...-->

|]
    where
    url' :: Text
    url' = urlQ0 ("package:" <> (resultName res))

renderPackage q (_,res) = [Hamlet.hamlet|
<div .panel .panel-default>
    $forall m <- res
        ^{renderModule q m}

|]

renderPagination :: Text -> Api.LimitedResult a -> Hamlet.HtmlUrl Routes
renderPagination query' lr = [Hamlet.hamlet|
<ul .pagination>
  $if isFirstPage
      <li .disabled>
          <span>&laquo;
  $else
      <li>
          <a href="#{urlQ'L query' leftArrowPage}">&laquo;
  $forall page <- pages
      $if page == currentPage
          <li .disabled>
              <span>#{page + 1}
      $else
          <li>
              <a href="#{urlQ'L query' page}">#{page + 1}
  $if isLastPage
      <li .disabled>
          <span>&raquo;
  $else
      <li>
          <a href="#{urlQ'L query' rightArrowPage}">&raquo;
          
|]
    where
        isFirstPage = currentPage == 0
        isLastPage = currentPage == lastPage
        page offset max' = offset `div` max'
        lastPage = page (Api.lrCount lr) (Api.lrMax lr)
        currentPage = page (Api.lrOffset lr) (Api.lrMax lr)

        firstPagerPage = (currentPage - 2) `max` 0
        lastPagerPage = ((currentPage + 2) `max` 5) `min` lastPage
        leftArrowPage = (firstPagerPage - 3) `max` 0 
        rightArrowPage = (lastPagerPage + 3) `min` lastPage 
        pages = [firstPagerPage .. lastPagerPage]
        


renderMergedLimitedResults :: Text -> Api.LimitedResult PackageResult -> Hamlet.HtmlUrl Routes
renderMergedLimitedResults query' lr = [Hamlet.hamlet|
$if null (Api.lrResult lr)
    <p>No results.
$else
    $forall result <- Api.lrResult lr
        ^{renderPackage query' result}
^{renderPagination query' lr}
|]


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

about :: Hamlet.HtmlUrl Routes
about = [Hamlet.hamlet|
<div .page-header>
  <h1>
      About Hayoo!
|]

examples :: Hamlet.HtmlUrl Routes
examples = [Hamlet.hamlet|
<div .page-header>
  <h1>
      Example Search Queries

<div .panel .panel-default>
    <div .panel-heading>
        <h3 .panel-title>
            If you don't find what you searched for by just searching for the name, you can try to search for specific properties by prefixing them
    <div .panel-body>
        <p>
            <a href="@{Home}?query=name%3AmapM">name:mapM
            searches for the function name mapM in all packages
        <p>
            <a href="@{Home}?query=package%3Abase">package:base
            searches for the base package.
        <p>
            <a href="@{Home}?query=a%20-%3E%20a">a -&gt; a
            searches for all functions with this signature in all packages.
        <p>
            <a href="@{Home}?query=module%3AControl.Exception">module:Control.Exception
            searches for a specific module in all packages.
<div .panel .panel-default>
    <div .panel-heading>
        <h3 .panel-title>
            It is also possible to combine search queries
    <div .panel-body>
        <p>
            <a href="@{Home}?query=package%3Abase%20mapM">package:base mapM
            searches for the function name mapM in the base package
        <p>
            <a href="@{Home}?query=mapM%20OR%20foldM">MapM or foldM
            searches will give a list of either MapM or foldM
        <p>
            <a href="@{Home}?query=map%20AND%20NOT%20package%3Abase">map AND NOT package:base
            searches for map, except for everything in the package base
<div .panel .panel-default>
    <div .panel-heading>
        <h3 .panel-title>
            You can also modify search queries
    <div .panel-body>
        <p>
            <a href="@{Home}?query=%22Map%20each%20element%22">"Map each element"
            searches for the string "Map each element"
        <p>
            <a href="@{Home}?query=%21mapM">!mapM
            searches case sensitive for mapM
        <p>
            <a href="@{Home}?query=~maMpaybe">~maMpaybe
            is a fuzzy search and will show mapMaybe

|]
