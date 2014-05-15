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

navigation :: Text -> Hamlet.HtmlUrl Routes
navigation q = [Hamlet.hamlet|
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
            <li>
                <a href=@{Examples}>Examples
            <li>
                <a href=@{About}>About
|]


footer :: Hamlet.HtmlUrl Routes
footer = [Hamlet.hamlet|
<footer id="footer">
    <a href=@{Home}> Hayoo Frontend
    &copy; 2014 Sebastian Philipp | Powered by 
    <a href="https://github.com/hunt-framework/hunt">
        Hunt
|]

body :: Text -> Hamlet.HtmlUrl Routes -> T.Text 
body q content = T.pack $ Blaze.renderHtml $ [Hamlet.hamlet|
$doctype 5
<html lang="en">
    ^{header q}
    <body>
        ^{navigation q}
        
        <div class="container">
            ^{content}
        
        ^{footer}
|] render

mainUri :: a -> a
mainUri m = m --snd $ head $ toList m

ajax :: Hamlet.HtmlUrl Routes -> T.Text 
ajax content = T.pack $ Blaze.renderHtml $ content render

-- ---------------------------------

renderBoxedResultHeading :: SearchResult -> Hamlet.HtmlUrl Routes
renderBoxedResultHeading r@(NonPackageResult {resultType=Method}) = [Hamlet.hamlet|
<div .panel-heading>
    <a href=#{mainUri $ resultUri r}>
        #{resultName r}
    :: #{resultSignature r}
    <span .label .label-default>
        Class Method
    ^{renderDropdown r}
|]

renderBoxedResultHeading r@(NonPackageResult {resultType=Function}) = [Hamlet.hamlet|
<div .panel-heading>
    <a href=#{mainUri $ resultUri r}>
        #{resultName r}
    :: #{resultSignature r}
    ^{renderDropdown r}
|]

renderBoxedResultHeading r@(NonPackageResult {}) = [Hamlet.hamlet|
<div .panel-heading>
    #{show $ resultType r}
    <a href=#{mainUri $ resultUri r}>
        #{resultName r}
    ^{renderDropdown r}
|]

renderBoxedResultHeading r@(PackageResult {}) = [Hamlet.hamlet|
<div .panel-heading>
    <a href=#{mainUri $ resultUri r}>
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

renderDropdown :: SearchResult -> Hamlet.HtmlUrl Routes
renderDropdown r = renderDropdown' r qs'
    where
    qs' = contextQueries r
    renderDropdown' r [] = [Hamlet.hamlet|
|]
    renderDropdown' r qs = [Hamlet.hamlet|
<div .pull-right .dropdown .text-primary id="fat-menu">
    <a href="#" id="drop3" role="button" .dropdown-toggle data-toggle="dropdown">
        More<b class="caret"></b>
        <ul .dropdown-menu role="menu" aria-labelledby="drop3">
            $forall (n, u) <- namedQueries
                <li role="presentation">
                    <a role="menuitem" tabindex="-1" href="#{u}">
                        #{n}
        

|]
        where
        namedQueries = map (\q -> (contextQueryName q, urlQ'S (printQuery $ contextQueryToQuery q r) 0)) qs

    
    


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
