# Revision history for hpath-directory

## 0.14.2.1 -- 2021-08-12

- Make compatible with streamly 0.8.0

## 0.14.1 -- ????-??-??

- add `readFileStrict`
- improve efficiency of `readFileStrict`

## 0.14.0 -- 2020-07-04

* Fix `readFile` to do proper lazy IO
  * change type signature of `readFileStream` slightly
* Fix some documentation

## 0.13.4 -- 2020-05-08

* Add getDirsFilesStream and use streamly-posix for dircontents (#34)

## 0.13.3 -- 2020-04-14

* Fix tests on mac

## 0.13.2 -- 2020-02-17

* Fix bug in `createDirRecursive` with trailing path separators

## 0.13.1 -- 2020-01-29

* Split some functionality out into 'hpath-posix'

## 0.1.0.0 -- 2020-01-26

* First version. Released on an unsuspecting world.
