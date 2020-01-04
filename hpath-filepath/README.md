# HPath-filepath

[![Gitter chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/hasufell/hpath?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Hackage version](https://img.shields.io/hackage/v/hpath-filepath.svg?label=Hackage)](https://hackage.haskell.org/package/hpath-filepath) [![Build Status](https://api.travis-ci.org/hasufell/hpath.png?branch=master)](http://travis-ci.org/hasufell/hpath) [![Hackage-Deps](https://img.shields.io/hackage-deps/v/hpath-filepath.svg)](http://packdeps.haskellers.com/feed?needle=hpath-filepath)

Support for bytestring based filepath manipulation, similar to 'filepath'.

## Motivation

This is basically a fork of [posix-paths](https://github.com/JohnLato/posix-paths), which seemed to have stalled development.

There is also a similar library [filepath-bytestring](https://hackage.haskell.org/package/filepath-bytestring), but it doesn't follow an open development model and is cross-platform, which this library is not interested in.

## Differences to 'posix-paths'

* uses the `word8` package for save word8 literals instead of `OverloadedStrings`
* `hasTrailingPathSeparator` and `dropTrailingPathSeparator` behave in the same way as their `System.FilePath` counterpart
* has some additional functions

## Differences to 'filepath-bytestring'

* uses the `word8` package for save word8 literals instead of `OverloadedStrings`
* is not cross-platform (less odd code to maintain)
* has some additional functions
