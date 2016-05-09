{-# LANGUAGE OverloadedStrings #-}

module HPath.IO.GetDirsFilesSpec where


import Data.List
  (
    sort
  )
import Data.Maybe
  (
    fromJust
  )
import qualified HPath as P
import Test.Hspec
import System.IO.Error
  (
    ioeGetErrorType
  )
import System.Posix.Env.ByteString
  (
    getEnv
  )
import GHC.IO.Exception
  (
    IOErrorType(..)
  )
import Utils
import qualified Data.ByteString as BS
import           Data.ByteString.UTF8 (toString)


ba :: BS.ByteString -> BS.ByteString -> BS.ByteString
ba = BS.append

specDir :: BS.ByteString
specDir = "test/HPath/IO/getDirsFilesSpec/"

specDir' :: String
specDir' = toString specDir


spec :: Spec
spec =
  describe "HPath.IO.getDirsFiles" $ do

    -- successes --
    it "getDirsFiles, all fine" $ do
      pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
      expectedFiles <- mapM P.parseRel [(specDir `ba ` ".hidden")
                                       ,(specDir `ba ` "Lala")
                                       ,(specDir `ba ` "dir")
                                       ,(specDir `ba ` "dirsym")
                                       ,(specDir `ba ` "file")
                                       ,(specDir `ba ` "noPerms")
                                       ,(specDir `ba ` "syml")]
      (fmap sort $ getDirsFiles' specDir)
        `shouldReturn` fmap (pwd P.</>) expectedFiles

    -- posix failures --
    it "getDirsFiles, nonexistent directory" $
      getDirsFiles' (specDir `ba ` "nothingHere")
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "getDirsFiles, wrong file type (file)" $
      getDirsFiles' (specDir `ba ` "file")
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "getDirsFiles, wrong file type (symlink to file)" $
      getDirsFiles' (specDir `ba ` "syml")
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "getDirsFiles, wrong file type (symlink to dir)" $
      getDirsFiles' (specDir `ba ` "dirsym")
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "getDirsFiles, can't open directory" $
      getDirsFiles' (specDir `ba ` "noPerms")
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)




