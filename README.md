# HPath

[![Gitter chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/hasufell/hpath?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Hackage version](https://img.shields.io/hackage/v/hpath.svg?label=Hackage)](https://hackage.haskell.org/package/hpath) [![Build Status](https://api.travis-ci.org/hasufell/hpath.png?branch=master)](http://travis-ci.org/hasufell/hpath)

Support for well-typed paths in Haskell. Also provides ByteString based filepath
manipulation.

## Motivation

The motivation came during development of
[hsfm](https://github.com/hasufell/hsfm)
which has a pretty strict File type, but lacks a strict Path type, e.g.
for user input.

The library that came closest to my needs was
[path](https://github.com/chrisdone/path),
but the API turned out to be oddly complicated for my use case, so I
decided to fork it.

Similarly, [posix-paths](https://github.com/JohnLato/posix-paths)
was exactly what I wanted for the low-level operations, but upstream seems dead,
so it is forked as well and merged into this library.

## Goals

* well-typed paths
* high-level API to file operations like recursive directory copy
* safe filepath manipulation, never using String as filepath, but ByteString
* still allowing sufficient control to interact with the underlying low-level calls

## Differences to 'path'

* doesn't attempt to fake IO-related information into the path, so whether a path points to a file or directory is up to your IO-code to decide...
* trailing path separators will be preserved if they exist, no messing with that
* uses safe ByteString for filepaths under the hood instead of unsafe String
* fixes broken [dirname](https://github.com/chrisdone/path/issues/18)
* renames dirname/filename to basename/dirname to match the POSIX shell functions
* introduces a new `Path Fn` for safe filename guarantees and a `RelC` class
* allows pattern matching via unidirectional PatternSynonym
* uses simple doctest for testing
* allows `~/` as relative path, because on posix level `~` is just a regular filename that does _NOT_ point to `$HOME`
* remove TH, it sucks

## Differences to 'posix-paths'

* uses the `word8` package for save word8 literals instead of `OverloadedStrings`
* `hasTrailingPathSeparator` and `dropTrailingPathSeparator` behave in the same way as their `System.FilePath` counterpart
* added various functions:
    * `equalFilePath`
    * `getSearchPath`
    * `hasParentDir`
    * `hiddenFile`
    * `isFileName`
    * `isValid`
    * `makeRelative`
    * `makeValid`
    * `normalise`
    * `splitSearchPath`
    * `stripExtension`
* has a custom versions of `openFd` which allows more control over the flags than its unix package counterpart
* adds a `getDirectoryContents'` version that works on Fd

