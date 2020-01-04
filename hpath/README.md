# HPath

[![Gitter chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/hasufell/hpath?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Hackage version](https://img.shields.io/hackage/v/hpath.svg?label=Hackage)](https://hackage.haskell.org/package/hpath) [![Build Status](https://api.travis-ci.org/hasufell/hpath.png?branch=master)](http://travis-ci.org/hasufell/hpath) [![Hackage-Deps](https://img.shields.io/hackage-deps/v/hpath.svg)](http://packdeps.haskellers.com/feed?needle=hpath)

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

## Goals

* well-typed paths
* safe filepath manipulation, never using String as filepath, but ByteString

Note: this library was written for __posix__ systems and it will probably not support other systems.

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