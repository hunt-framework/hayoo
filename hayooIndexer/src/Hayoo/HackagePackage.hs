module Hayoo.HackagePackage
where

import           Data.List             (nub, sort)

import           Hayoo.PackageInfo     (PackageInfo, mkPackageInfo)
import           Hayoo.URIConfig       (hackagePackages, fileName)


import           Holumbus.Crawler      (match)
import           Holumbus.Crawler.Html (getAllText, getByPath, getHtmlTitle, isElemWithAttr)

import           Text.XML.HXT.Core     (LA, IOSArrow, XmlTree,
                                        (&&&), (>>>), (/>), (>>.), (>>^),
                                        arr,
                                        deep,
                                        fromLA,
                                        getChildren, getText, guards,
                                        hasName, hasText, hasAttrValue,
                                        listA,
                                        neg, none,
                                        single, spanA,
                                        this, transferURI,
                                        unlistA,
                                        when, withDefault)

-- ------------------------------------------------------------

hayooGetPkgInfo                 :: IOSArrow XmlTree PackageInfo
hayooGetPkgInfo                 = fromLA $
                                  ( getPkgName
                                    &&&
                                    getPkgVersion
                                    &&&
                                    getPkgDependencies
                                    &&&
                                    getPkgAuthor
                                    &&&
                                    getPkgMaintainer
                                    &&&
                                    getPkgCategory
                                    &&&
                                    getPkgHomepage
                                    &&&
                                    getPkgSynopsis
                                    &&&
                                    getPkgDescr
                                    &&&
                                    getUploadDate
                                  )
                                  >>^
                                  ( \(x1, (x2, (x3, (x4, (x5, (x6, (x7, (x8, (x9, x10))))))))) ->
                                    mkPackageInfo x1 x2 x3 x4 x5 x6 x7 x8 x9 x10
                                  )

hayooGetPkgTitle                :: IOSArrow XmlTree String
hayooGetPkgTitle                = fromLA $
                                  getPkgName

-- ------------------------------------------------------------
--
-- all the get arrows must be deterministic
-- else hayooGetPkgInfo fails, which is an error

getPkgName                      :: LA XmlTree String
getPkgName                      = getHtmlTitle
                                  >>^
                                  ( -- select the package name, the 1. word in title
                                    words
                                    >>> take 1
                                    >>> unwords

                                    -- and remove trailing ":" if there is any ":"
                                    >>> reverse
                                    >>> dropColon
                                    >>> reverse
                                  )
    where
      dropColon (':' : n)       = n
      dropColon n               = n

getPkgSynopsis                  :: LA XmlTree String
getPkgSynopsis                  = getHtmlTitle
                                  >>^
                                  ( words >>> drop 1
                                    >>>
                                    reverse >>> drop 2 >>> reverse
                                    >>>
                                    unwords
                                  )

getPkgVersion                   :: LA XmlTree String
getPkgVersion                   = getAllText
                                  $
                                  getProperty "Versions"
                                  >>>
                                  getChildren
                                  >>>
                                  hasName "strong"

getPkgDependencies              :: LA XmlTree [String]
getPkgDependencies              = ( getProperty "Dependencies"
                                    >>>
                                    listA
                                    ( getChildren
                                      >>>
                                      hasName "a"
                                      >>>
                                      getChildren
                                      >>>
                                      getText
                                      >>>
                                      this -- checkPackageName
                                    )
                                    >>>
                                    arr (sort >>> nub)
                                  ) `withDefault` []

getPkgAuthor                    :: LA XmlTree String
getPkgAuthor                    = getAllText $ getProperty "Author(s)?"

getPkgMaintainer                :: LA XmlTree String
getPkgMaintainer                = getAllText $ getProperty "Maintainer(s)?"

getPkgCategory                  :: LA XmlTree String
getPkgCategory                  = getAllText $ getProperty "Category"

getPkgHomepage                  :: LA XmlTree String
getPkgHomepage                  = getAllText $ getProperty "Home page"

getUploadDate                   :: LA XmlTree String
getUploadDate                   = getAllText $ getProperty "Upload date"

getPkgDescr                     :: LA XmlTree String
getPkgDescr                     = getAllText
                                  -- take all stuff between "h1" and next "h2" element in content
                                  ( ( getByPath ["html","body"]
                                      />
                                      isElemWithAttr "div" "id" (== "content")
                                    )
                                    >>>
                                    listA getChildren
                                    >>>
                                    spanA (neg $ hasName "h2")
                                    >>>
                                    arr fst
                                    >>>
                                    unlistA
                                    >>>
                                    (none                       -- remove h1 header
                                     `when` hasName "h1")
                                    >>>
                                    (none
                                     `when` hasName "ul")       -- remove changelog
                                    >>>
                                    (none                       -- remove "Tags: ..." part
                                     `when` isElemWithAttr "div" "style" (== "font-size: small"))
                                  )

-- ------------------------------------------------------------

preparePkg                      :: IOSArrow XmlTree XmlTree
preparePkg                      = fromLA $
                                  isHackagePackage

isHackagePackage                :: LA XmlTree XmlTree
isHackagePackage                = hasAttrValue transferURI (match $ hackagePackages ++ fileName)

-- ------------------------------------------------------------

getProperties           :: LA XmlTree XmlTree
getProperties           = single (deep (hasName "table"))               -- there should be only a single table
                                                                        -- old package layout:
                                                                        -- deep ( isElemWithAttr "table" "class" (== "properties") )
                          />
                          hasName "tr"

getProperty             :: String -> LA XmlTree XmlTree
getProperty kw          = getProperties
                          >>>
                          ( ( getChildren
                              >>>
                              hasName "th"
                              >>>
                              ( getChildren >>. take 1 )
                              >>>
                              hasText (match kw)
                            )
                            `guards`
                            ( getChildren
                              >>>
                              hasName "td"
                            )
                          )

-- ------------------------------------------------------------
