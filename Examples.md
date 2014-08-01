Example Search Queries
======================

If you did not find what you searched for, you can try to search for specific properties by prefixing them.

[name:mapM](/?query=name:mapM) searches for the function name mapM in all packages.

[package:base](/?query=package:base) searches for the base package.

[a -> a](/?query=a -> a) searches for functions with this signature in all packages.

[module:Control.Exception](/?query=module:Control.Exception) searches for a specific module in all packages.

Combination
-----------

It is also possible to combine search queries:

[package:base mapM](/?query=package:base mapM) searches for the function name mapM in the base package.

[MapM or foldM](/?query=mapM OR foldM) searches for either MapM or foldM.

[map AND NOT package:base](/?query=map AND NOT package:base) searches for map, but not in the base package.

Modification
------------

You can also modify search queries:

["Map each element"](/?query=%22Map each element%22) searches for the string "Map each element".

[!mapM](/?query=%21mapM) searches case-sensitive for mapM.

<!--[~maMpaybe](/?query=~maMpaybe) is a fuzzy search and will show mapMaybe-->

Range
-----

["upload:[2014-01-01T00:00:00 TO 2015-01-01T00:00:00]"](/?query=upload:[2014-01-01T00:00:00 TO 2015-01-01T00:00:00]) searches for all packages, that were uploaded in 2014.
