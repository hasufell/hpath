# HPath-filepath

[![Gitter chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/hasufell/hpath?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Hackage version](https://img.shields.io/hackage/v/hpath-directory.svg?label=Hackage)](https://hackage.haskell.org/package/hpath-directory) [![Build Status](https://api.travis-ci.org/hasufell/hpath.png?branch=master)](http://travis-ci.org/hasufell/hpath) [![Hackage-Deps](https://img.shields.io/hackage-deps/v/hpath-directory.svg)](http://packdeps.haskellers.com/feed?needle=hpath-directory)

Support high-level IO operations on files/directories, utilizing ByteString
as FilePaths.

This package is part of the HPath suite, also check out:

* [hpath](https://hackage.haskell.org/package/hpath)
* [hpath-filepath](https://hackage.haskell.org/package/hpath-filepath)
* [hpath-io](https://hackage.haskell.org/package/hpath-io)

## Motivation

This is basically a fork of [directory](https://hackage.haskell.org/package/directory), but is a complete rewrite and the API doesn't follow the directory package.

## Differences to 'posix-paths'

* has a custom versions of `openFd` which allows more control over the flags than its unix package counterpart
* adds a `getDirectoryContents'` version that works on Fd
