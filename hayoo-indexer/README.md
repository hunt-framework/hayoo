# Hayoo Indexer

This package provides an indexer for the Hayoo! search engine, based
on Hoogle and Cabal files.


## How do I use this?

After building this package, you should be able to get up and running
with indexing Hackage, using the following commands.

```bash
$ hayoo index
```

This command will download the relevant .tar.gz files from Hackage,
process them, store the result in a directory called `index/` and
delete the previously downloaded artifacts. Beware, this may take a
moment.

To be able to run Hayoo!, you need to pump the index into an instance
of Hunt. If you have Hunt running already, this should be as simple as:

```bash
$ hayoo pump
```

If you don't know what Hunt is, why you need it or how to start it,
please refer to [these docs][1], to learn all you need. We will wait for
you to come back... *whistling*.

Are you back? Great! Did you run the `hayoo pump` command? Very well!
Now that you have Hunt running, the only thing left to do is starting
the Hayoo! server.

```bash
$ hayoo server
```

Now go to [http://localhost:3001][2] and you should be able to start
searching. 

If you want to customize any directories, ports or other options, just
use the `--help` flag for any of the shown commands. This should get
you up to speed on what you are able to customize.


## Goals

Now that you know, how to run Hayoo!, let's take a look at what our
goals with this particular implementation of the indexer are and why.


### Incremental indexing

There are two ways to interpret the term *incremental indexing*. The
first means simply, if the current indexing process will be stopped by
the user or a system failure, it should pick up indexing, right where
it left. The other meaning is, once fully indexed, only *updates*
should be the result of an indexing run. 

While the former is a worthwhile idea to keep in mind while
implementing the indexer, alas leaving room for this implementation
detail, the latter is the meaning, we are aiming for here. Not only
should this allow for much speedier indexing the second time around,
it should also make it simpler and faster to `pump` just updates into
Hunt.


### Design for simplicity

Simplicity lies in the eye of the beholder, of course. Using the
indexer though, should be as simple as two commands to just get up and
running quickly and simply, while still maintaining the ability to
customize important parameters. As you should have seen in [How do I
use this?][3], 


### Future proof

Indexing may not always be done with Hoogle and Cabal
files. Fundamentally the indexer is an interactive program. This means
indexing will always need specific files to be run in the first place,
which always need to leak through to the user. What should not change,
however, is the logic for incrementally indexing files.

Furthermore, it should be very easy to see, which changes need to be
made, to support another way to get the data in the first place.


### Good errors

In the spirit of the rising good error messages, in [Elm][4],
[Rust][5], we want good reporting and error messages, if something
goes south.



## Implementation details

In this section, we will discuss the overarching ideas, mainly for
incremental indexing.

### Incremental indexing

The idea here is fundamentally simple and understanding it, will
require mostly just looking at the file system. After the first time
ever indexing anything, the `index/*` directory will contain the
following directory structure.


```bash
$ tree index/
index/
└── indexed-201712201005
    ├── index.json
    ├── declarations.tar.gz
    ├── packages.tar.gz
    └── schema.json
```

`index.json` is the most interesting file and contains all information
about a given indexer run. It looks like this.


```
{
  "startedAt": "2017-12-20T10:00:00",
  "finishedAt": "2017-12-20T10:00:00",
  "errors": [],
  "declarations": {
    "chunks": [{
      "filename": "d41d8cd98f00b204e9800998ecf8427e.json",
      "packages": [{
        "name": "aeson",
        "version": "1.2.3.0"
      }, {
        "name": "servant",
        "version": "0.12"
      }]
    }]
  },
  "packages": {
    "chunks": [{
      "filename": "d41d8cd98f00b204e9800998ecf8427e.json",
      "packages": [{
        "name": "aeson",
        "version": "1.2.3.0"
      }]
    }]
  }
}
```


The `index.json` contains enough information, to generate a small
report about the indexer run. This can be done with the following
command.

```bash
$ hayoo index report --latest
```

Now, if we go for indexing Hackage a second time, we can use the
`index.json` file to compare them with packages, which haven't changed
and therefore need not be re-indexed. If there are relevant updates,
the following file structure, will be the result of the second run.

```bash
$ tree index/
index/
└── indexed-201712201005
    ├── index.json
    ├── declarations.tar.gz
    ├── packages.tar.gz
    └── schema.json
└── indexed-201712201105
    ├── index.json
    ├── declarations.tar.gz
    └── packages.tar.gz
```

The idea is, that we will never change the first index and the second
index, will only contain updated packages.


[1]: Hunt README
[2]: http://localhost:3000
[3]: #how-do-i-use-this
