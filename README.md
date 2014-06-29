About
=====

What is Hayoo
-------------

Hayoo is a search engine for [Hackage](http://hackage.haskell.org/) packages. It is possible to search for functions, data types and packages. Each indexed document consists of a name, a description and, if applicable, a signature. Have a look at the [examples](/examples) of how to use Hayoo. All available packages are indexed in contrast to Hoogle, although the signature search is not as advances as the one from Hoogle. 

Hunt
----

Hayoo uses [Hunt](huntsearch.org) for indexing and searching. Hunt is a flexible, lightweight search platform with a powerful query language and JSON api. Hayoo exposes the query language through its interface and thus allows to execute advanced queries on Hayoo. 

Additionally, Hunt consists of a stand alone web server to allow integration into existing infrastructures without the need of writing Haskell code.


Json api
--------

Hayoo exposes a Json api for the usage of other clients. The json api is available under [/json](/json?query=Monad). The json api accepts a parameter [page](/json?query=Monad&page=2) to cycle through the pages. 

Feedback
--------

Bug-reports should be submitted to [Github](https://github.com/hunt-framework/hayoo/issues), You can also provide Feedback to [info@huntsearch.org](mailto:info@huntsearch.org)

History
-------

The old Hayoo is based on the Holumbus search engine. Hunt is the successor of Holumbus and the new Hayoo is a rewrite on top of the Hunt search engine. 

