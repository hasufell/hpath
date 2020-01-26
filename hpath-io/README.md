# HPath-IO

[![Gitter chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/hasufell/hpath?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Hackage version](https://img.shields.io/hackage/v/hpath-io.svg?label=Hackage)](https://hackage.haskell.org/package/hpath-io) [![Build Status](https://api.travis-ci.org/hasufell/hpath.png?branch=master)](http://travis-ci.org/hasufell/hpath) [![Hackage-Deps](https://img.shields.io/hackage-deps/v/hpath-io.svg)](http://packdeps.haskellers.com/feed?needle=hpath-io)

High-level IO operations on files/directories, utilizing type-safe Paths. This uses [hpath-directory](https://hackage.haskell.org/package/hpath-directory) under the hood.

This package is part of the HPath suite, also check out:

* [hpath](https://hackage.haskell.org/package/hpath)
* [hpath-directory](https://hackage.haskell.org/package/hpath-directory)
* [hpath-filepath](https://hackage.haskell.org/package/hpath-filepath)

## Motivation

The motivation came during development of
[hsfm](https://github.com/hasufell/hsfm)
in order to have a proper high-level API of file related operations,
while utilizing type-safe Paths.

## Goals

* high-level API to file operations like recursive directory copy
* still allowing sufficient control to interact with the underlying low-level calls
* unit-testing exceptions (because yes, people may rely on them)

Note: this library was written for __posix__ systems and it will probably not support other systems.

