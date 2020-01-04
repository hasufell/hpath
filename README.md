# HPath libraries

[![Gitter chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/hasufell/hpath?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Build Status](https://api.travis-ci.org/hasufell/hpath.png?branch=master)](http://travis-ci.org/hasufell/hpath)

Set of libraries to deal with filepaths and files.

## Motivation

* filepaths should be type-safe (absolute, relative, ...)
* filepaths should be ByteString under the hood, see [Abstract FilePath Proposal (AFPP)](https://gitlab.haskell.org/ghc/ghc/wikis/proposal/abstract-file-path)
* file high-level operations should be platform-specific, exception-stable, safe and as atomic as possible

## Projects

* [hpath](./hpath): Support for well-typed paths
* [hpath-filepath](./hpath-filepath): ByteString based filepath manipulation (can be used without hpath)
* [hpath-io](./hpath-io): high-level file API (recursive copy, writeFile etc.) using hpath
