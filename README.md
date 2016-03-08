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

I wasn't happy with the way it dealt with Dir vs File things. In his
version of the library, a `Path b Dir` always ends with a trailing
path separator and `Path b File` never ends with a trailing path separator.

IMO, it is nonsensical to make a Dir vs File distinction on path level,
although it first seems nice.
Some of the reasons are:
* a path is just that: a path. It is completely disconnected from IO level
  and even if a `Dir`/`File` type theoretically allows us to say "this path
  ought to point to a file", there is literally zero guarantee that it will
  hold true at runtime. So this basically gives a false feeling of a
  type-safe file distinction.
* it's imprecise about Dir vs File distinction, which makes it even worse,
  because a directory is also a file (just not a regular file). Add symlinks
  to that and the confusion is complete.
* it makes the API oddly complicated for use cases where we basically don't
  care (yet) whether something turns out to be a directory or not

Still, it comes also with a few perks:
* it simplifies some functions, because they now have guarantees whether a
  path ends in a trailing path separator or not
* it may be safer for interaction with other library functions, which behave
  differently depending on a trailing path separator (like probably shelly)

Not limited to, but also in order to fix my remarks without breaking any
benefits, I did:
* rename the `Dir`/`File` types to `TPS`/`NoTPS`, so it's clear we are only
  giving information about trailing path separators and not actual file
  types we don't know about yet
* add a `MaybeTPS` type, which does not mess with trailing path separators
  and also gives no guarantees about them... then added `toNoTPS` and
  `toTPS` to allow type-safe conversion
* make some functions accept more general types, so we don't unnecessarily
  force paths with trailing separators for `(</>)` for example... instead
  these functions now examine the paths to still have correct behavior.
  This is really minor overhead. You might say now "but then I can append
  filepath to filepath". Well, as I said... we don't know whether it's a
  "filepath" at all.
* merge `filename` and `dirname` into `basename` and make `parent` be
  `dirname`, so the function names match the name of the POSIX ones,
  which do (almost) the same...
* fix a bug in `basename` (formerly `dirname`) which broke the type
  guarantees
* add a pattern synonym for easier pattern matching without exporting
  the internal Path constructor

## Consequences

So what does that mean? Well, it means that this library does not and
cannot make any guarantees about what a filepath is meant for or what
it might point to. And it doesn't pretend it can.

So when you strip the trailing path separator of a path that points to a
directory  and then shove it into some function which expects a regular
file... then that function will very likely blow up. That's the nature of IO.
There is no type that can save you from interfacing such low-level libraries.
The filesystem is in constant change. What might have been a regular file
2 seconds ago, can now be a directory or a symlink.
That means you need a proper File type that is tied to your IO code.
This is what [hsfm](https://github.com/hasufell/hsfm) does. It currently
is not a library, maybe it will be in the future.

