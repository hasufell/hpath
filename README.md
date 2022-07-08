# HPath libraries

[![Gitter chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/hasufell/hpath?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Build Status](https://api.travis-ci.org/hasufell/hpath.png?branch=master)](http://travis-ci.org/hasufell/hpath)

Set of libraries to deal with filepaths and files.

## Motivation

* filepaths should be type-safe (absolute, relative, ...)
* filepaths should be ByteString under the hood, see [Abstract FilePath Proposal (AFPP)](https://gitlab.haskell.org/ghc/ghc/wikis/proposal/abstract-file-path)
* file high-level operations should be platform-specific, exception-stable, safe and as atomic as possible

## Projects

* [![Hackage version](https://img.shields.io/hackage/v/hpath.svg?label=Hackage)](https://hackage.haskell.org/package/hpath) [hpath](./hpath): Support for well-typed paths
* [![Hackage version](https://img.shields.io/hackage/v/hpath-directory.svg?label=Hackage)](https://hackage.haskell.org/package/hpath-directory) [hpath-directory](./hpath-directory): High-level IO operations for files/directories on raw ByteString filepaths (use hpath-io for the type-safe path version)
* [![Hackage version](https://img.shields.io/hackage/v/hpath-io.svg?label=Hackage)](https://hackage.haskell.org/package/hpath-io) [hpath-io](./hpath-io): High-level IO operations for files/directories utilizing type-safe Path
* [![Hackage version](https://img.shields.io/hackage/v/hpath-posix.svg?label=Hackage)](https://hackage.haskell.org/package/hpath-posix) [hpath-posix](./hpath-posix): Some low-level POSIX glue code that is not in 'unix'
