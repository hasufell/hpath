# HPath

Support for well-typed paths in Haskell.

## Motivation

The motivation came during development of
[hsfm](https://github.com/hasufell/hsfm)
which has a pretty strict File type, but lacks a strict Path type, e.g.
for user input.

The library that came closest to my needs was
[path](https://github.com/chrisdone/path),
but the API turned out to be oddly complicated for my use case, so I
decided to fork it.

## Differences to 'path'

* doesn't attempt to fake IO-related types into the path, so whether a path points to a file or directory is up to your IO-code to decide... this should be a library that is used _with_ a proper IO File Type
* trailing path separators will be preserved if they exist, no messing with that
* uses safe ByteString for filepaths under the hood instead of unsafe String
* fixes broken [dirname](https://github.com/chrisdone/path/issues/18)
* renames dirname/filename to basename/dirname to match the POSIX shell functions
* introduces a new `Path Fn` for safe filename guarantees and a `RelC` class
* allows pattern matching via unidirectional PatternSynonym
* uses simple doctest for testing
* allows `~/` as relative path, because on posix level `~` is just a regular filename that does _NOT_ point to `$HOME`
* remove TH, it sucks

