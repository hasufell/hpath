{-# LANGUAGE TemplateHaskell #-}

-- | Test suite.

module Main where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid
import HPath
import HPath.Internal
import Test.Hspec

-- | Test suite entry point, returns exit failure if any test fails.
main :: IO ()
main = hspec spec

-- | Test suite.
spec :: Spec
spec =
  do describe "Parsing: Path Abs Dir" parseAbsTPSSpec
     describe "Parsing: Path Rel Dir" parseRelTPSSpec
     describe "Parsing: Path Abs File" parseAbsNoTPSSpec
     describe "Parsing: Path Rel File" parseRelNoTPSSpec
     describe "Operations: (</>)" operationAppend
     describe "Operations: stripDir" operationStripDir
     describe "Operations: isParentOf" operationIsParentOf
     describe "Operations: dirname" operationDirname
     describe "Operations: basename" operationBasename
     describe "Restrictions" restrictions

-- | Restricting the input of any tricks.
restrictions :: Spec
restrictions =
  do parseFails "~/"
     parseFails "~/foo"
     parseFails "~/foo/bar"
     parseFails "../"
     parseFails ".."
     parseFails "."
     parseFails "/.."
     parseFails "/foo/../bar/"
     parseFails "/foo/bar/.."
  where parseFails x =
          it (show x ++ " should be rejected")
             (isNothing (void (parseAbsTPS x) <|>
                         void (parseRelTPS x) <|>
                         void (parseAbsNoTPS x) <|>
                         void (parseRelNoTPS x)))

-- | The 'basename' operation.
operationBasename :: Spec
operationBasename =
  do it "basename ($(mkAbsTPS parent) </> basename $(mkRelNoTPS filename)) == $(mkRelNoTPS filename)"
        ((basename =<< ($(mkAbsTPS "/home/hasufell/") </>)
                        <$> basename $(mkRelNoTPS "bar.txt")) ==
         Just $(mkRelNoTPS "bar.txt"))
     it "basename ($(mkRelTPS parent) </> basename $(mkRelNoTPS filename)) == $(mkRelNoTPS filename)"
        ((basename =<< ($(mkRelTPS "home/hasufell/") </>)
                        <$> basename $(mkRelNoTPS "bar.txt")) ==
         Just $(mkRelNoTPS "bar.txt"))

-- | The 'dirname' operation.
operationDirname :: Spec
operationDirname =
  do it "dirname (parent </> child) == parent"
        (dirname ($(mkAbsTPS "/foo") </>
                    $(mkRelTPS "bar")) ==
         $(mkAbsTPS "/foo"))
     it "dirname \"\" == \"\""
        (dirname $(mkAbsTPS "/") ==
         $(mkAbsTPS "/"))
     it "dirname (parent \"\") == \"\""
        (dirname (dirname $(mkAbsTPS "/")) ==
         $(mkAbsTPS "/"))

-- | The 'isParentOf' operation.
operationIsParentOf :: Spec
operationIsParentOf =
  do it "isParentOf parent (parent </> child)"
        (isParentOf
           $(mkAbsTPS "///bar/")
           ($(mkAbsTPS "///bar/") </>
            $(mkRelNoTPS "bar/foo.txt")))
     it "isParentOf parent (parent </> child)"
        (isParentOf
           $(mkRelTPS "bar/")
           ($(mkRelTPS "bar/") </>
            $(mkRelNoTPS "bob/foo.txt")))

-- | The 'stripDir' operation.
operationStripDir :: Spec
operationStripDir =
  do it "stripDir parent (parent </> child) = child"
        (stripDir $(mkAbsTPS "///bar/")
                  ($(mkAbsTPS "///bar/") </>
                   $(mkRelNoTPS "bar/foo.txt")) ==
         Just $(mkRelNoTPS "bar/foo.txt"))
     it "stripDir parent (parent </> child) = child"
        (stripDir $(mkRelTPS "bar/")
                  ($(mkRelTPS "bar/") </>
                   $(mkRelNoTPS "bob/foo.txt")) ==
         Just $(mkRelNoTPS "bob/foo.txt"))
     it "stripDir parent parent = _|_"
        (stripDir $(mkAbsTPS "/home/hasufell/foo")
                  $(mkAbsTPS "/home/hasufell/foo") ==
         Nothing)

-- | The '</>' operation.
operationAppend :: Spec
operationAppend =
  do it "AbsDir + RelDir = AbsDir"
        ($(mkAbsTPS "/home/") </>
         $(mkRelTPS "hasufell") ==
         $(mkAbsTPS "/home/hasufell/"))
     it "AbsDir + RelFile = AbsFile"
        ($(mkAbsTPS "/home/") </>
         $(mkRelNoTPS "hasufell/test.txt") ==
         $(mkAbsNoTPS "/home/hasufell/test.txt"))
     it "RelDir + RelDir = RelDir"
        ($(mkRelTPS "home/") </>
         $(mkRelTPS "hasufell") ==
         $(mkRelTPS "home/hasufell"))
     it "RelDir + RelFile = RelFile"
        ($(mkRelTPS "home/") </>
         $(mkRelNoTPS "hasufell/test.txt") ==
         $(mkRelNoTPS "home/hasufell/test.txt"))

-- | Tests for the tokenizer.
parseAbsTPSSpec :: Spec
parseAbsTPSSpec =
  do failing ""
     failing "./"
     failing "~/"
     failing "foo.txt"
     succeeding "/" (MkPath "/")
     succeeding "//" (MkPath "/")
     succeeding "///foo//bar//mu/" (MkPath "/foo/bar/mu/")
     succeeding "///foo//bar////mu" (MkPath "/foo/bar/mu/")
     succeeding "///foo//bar/.//mu" (MkPath "/foo/bar/mu/")
  where failing x = parserTest parseAbsTPS x Nothing
        succeeding x with = parserTest parseAbsTPS x (Just with)

-- | Tests for the tokenizer.
parseRelTPSSpec :: Spec
parseRelTPSSpec =
  do failing ""
     failing "/"
     failing "//"
     failing "~/"
     failing "/"
     failing "./"
     failing "././"
     failing "//"
     failing "///foo//bar//mu/"
     failing "///foo//bar////mu"
     failing "///foo//bar/.//mu"
     succeeding "..." (MkPath ".../")
     succeeding "foo.bak" (MkPath "foo.bak/")
     succeeding "./foo" (MkPath "foo/")
     succeeding "././foo" (MkPath "foo/")
     succeeding "./foo/./bar" (MkPath "foo/bar/")
     succeeding "foo//bar//mu//" (MkPath "foo/bar/mu/")
     succeeding "foo//bar////mu" (MkPath "foo/bar/mu/")
     succeeding "foo//bar/.//mu" (MkPath "foo/bar/mu/")
  where failing x = parserTest parseRelTPS x Nothing
        succeeding x with = parserTest parseRelTPS x (Just with)

-- | Tests for the tokenizer.
parseAbsNoTPSSpec :: Spec
parseAbsNoTPSSpec =
  do failing ""
     failing "./"
     failing "~/"
     failing "./foo.txt"
     failing "/"
     failing "//"
     failing "///foo//bar//mu/"
     succeeding "/..." (MkPath "/...")
     succeeding "/foo.txt" (MkPath "/foo.txt")
     succeeding "///foo//bar////mu.txt" (MkPath "/foo/bar/mu.txt")
     succeeding "///foo//bar/.//mu.txt" (MkPath "/foo/bar/mu.txt")
  where failing x = parserTest parseAbsNoTPS x Nothing
        succeeding x with = parserTest parseAbsNoTPS x (Just with)

-- | Tests for the tokenizer.
parseRelNoTPSSpec :: Spec
parseRelNoTPSSpec =
  do failing ""
     failing "/"
     failing "//"
     failing "~/"
     failing "/"
     failing "./"
     failing "//"
     failing "///foo//bar//mu/"
     failing "///foo//bar////mu"
     failing "///foo//bar/.//mu"
     succeeding "..." (MkPath "...")
     succeeding "foo.txt" (MkPath "foo.txt")
     succeeding "./foo.txt" (MkPath "foo.txt")
     succeeding "././foo.txt" (MkPath "foo.txt")
     succeeding "./foo/./bar.txt" (MkPath "foo/bar.txt")
     succeeding "foo//bar//mu.txt" (MkPath "foo/bar/mu.txt")
     succeeding "foo//bar////mu.txt" (MkPath "foo/bar/mu.txt")
     succeeding "foo//bar/.//mu.txt" (MkPath "foo/bar/mu.txt")
  where failing x = parserTest parseRelNoTPS x Nothing
        succeeding x with = parserTest parseRelNoTPS x (Just with)

-- | Parser test.
parserTest :: (Show a1,Show a,Eq a1)
           => (a -> Maybe a1) -> a -> Maybe a1 -> SpecWith ()
parserTest parser input expected =
  it ((case expected of
         Nothing -> "Failing: "
         Just{} -> "Succeeding: ") <>
      "Parsing " <>
      show input <>
      " " <>
      case expected of
        Nothing -> "should fail."
        Just x -> "should succeed with: " <> show x)
     (actual == expected)
  where actual = parser input
