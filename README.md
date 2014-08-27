# packrank: rank Haskell packages by usage

`packrank` is a library and command for ranking Haskell packages based
on how widely they are used.


# Installation

```
cabal update
cabal install -j packrank
```


# Usage

If you run the `packrank` command with no options, it will print a
list of *all* packages on Hackage, from least to most widely used.

The output will look something like this:

```
text 582
mtl 809
transformers 907
containers 973
deepseq 1286
bytestring 1299
array 1396
ghc-prim 9195
base 10000
```

The number after each package name has **no objective meaning**. It is
simply a scale on which the most widely used package is assigned a
score of 10,000. Why 10,000? This allows us to avoid printing out
floating point numbers, which are much more difficult to read than
integers.

If you run `packrank` with the name of a package, it will rank
only packages that transitively depend on the named package.  For
example, here are the top ten packages that depend on `attoparsec` (as
printed by `packrank attoparsec`):

```
io-streams 1045
configurator 1071
yaml 1204
persistent 1246
conduit-extra 1282
xml-conduit 1301
fay 1436
snap 1467
snap-core 1728
aeson 10000
```


# Behind the scenes

The mechanism that `packrank` relies on to measure use is simply the
Hackage dependency graph, which is conveniently cached locally on your
machine if you're a Haskell developer.  The definition of "wide use"
is recursive: a package is widely used if it is depended upon by other
widely-used packages.

This way of measuring use cannot take into account some important data
sources, so I don't claim that it is perfect.

* The number of times a package is downloaded from Hackage is not easy
  to obtain, but it is in any case not obvious how it should affect
  the ranking.

* Since executable-only packages on Hackage are not depended on by any
  other package, they do not influence the rankings at all.
