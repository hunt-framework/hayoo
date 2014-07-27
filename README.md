About
=====

What is Hayoo
-------------

Hayoo is a search engine for [Hackage][hackage] packages. It is possible to search for functions, data types and packages. Each indexed document consists of a name, a description and, if applicable, a signature. Have a look at the [examples](/examples) of how to use Hayoo. All available packages are indexed in contrast to Hoogle, although the signature search is not as advances as the one from Hoogle. 

Hunt
----

Hayoo uses [Hunt][huntsearch] for indexing and searching. Hunt is a flexible, lightweight search platform with a powerful query language and JSON API. Hayoo exposes the query language through its interface and thus allows to execute advanced queries on Hayoo. 

Additionally, Hunt consists of a stand alone web server to allow integration into existing infrastructures without the need of writing Haskell code.


JSON API
--------

Hayoo exposes a JSON API for the usage of other clients. The JSON API is available under [/json](/json?query=Monad). The JSON API accepts a parameter [page](/json?query=Monad&page=2) to cycle through the pages. 

Feedback
--------

Bug-reports should be submitted to [Github][hayoo-issues]. You can also provide Feedback to [hayoo2@fh-wedel.de](mailto:hayoo2@fh-wedel.de) 

History
-------

The old Hayoo! was based on the [Holumbus search engine][holumbus-github], which was developed in 2008-2009 by [Timo B. Kranz](//github.com/tbk303) and [Sebastian M. Gauck](https://twitter.com/sgauck).

The new Hayoo is a rewrite on top of the Hunt search engine, the successor of Holumbus, which was started in 2013 by [Ulf Sauer](//github.com/ulfS) and [Chris Reumann](//github.com/chrisreu) to improve and extend the
existing Holumbus framework. 

Both projects were developed at the [FH Wedel](http://fh-wedel.de) under supervision and active support of
[Prof. Dr. Uwe Schmidt](http://fh-wedel.de/~si).

The old [Hayoo][hayoo-alt] and [Holumbus][holumbus-alt] are still online.


[huntsearch]: http://huntsearch.org "huntsearch.org"
[hackage]: http://hackage.haskell.org/ "Hackage"
[hayoo-issues]: https://github.com/hunt-framework/hayoo/issues "Hayoo Issues"

[holumbus-github]: //github.com/fortytools/holumbus "Holumbus on GitHub"
[hayoo-alt]: http://holumbus-alt.fh-wedel.de/hayoo/hayoo.html "Old Hayoo"
[holumbus-alt]: http://holumbus-alt.fh-wedel.de/trac "Holumbus"
