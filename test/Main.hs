{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Runner
import Test.Hspec.Formatters
import qualified Spec
import Utils


-- TODO: chardev, blockdev, namedpipe, socket


main :: IO ()
main =
  hspecWith
    defaultConfig { configFormatter = Just progress }
    $ before_ fixPermissions
    $ after_ revertPermissions
    $ Spec.spec >> Spec.spec
  where
    noWriteDirs =  ["test/HPath/IO/copyFileSpec/outputDirNoWrite"
                   ,"test/HPath/IO/copyFileOverwriteSpec/outputDirNoWrite"
                   ,"test/HPath/IO/copyDirRecursiveSpec/noWritePerm"
                   ,"test/HPath/IO/copyDirRecursiveOverwriteSpec/noWritePerm"
                   ,"test/HPath/IO/createDirSpec/noWritePerms"
                   ,"test/HPath/IO/createRegularFileSpec/noWritePerms"
                   ,"test/HPath/IO/renameFileSpec/noWritePerm"
                   ,"test/HPath/IO/moveFileSpec/noWritePerm"
                   ,"test/HPath/IO/moveFileOverwriteSpec/noWritePerm"
                   ,"test/HPath/IO/recreateSymlinkSpec/noWritePerm"
                   ]
    noPermsDirs =  ["test/HPath/IO/copyFileSpec/noPerms"
                   ,"test/HPath/IO/copyFileOverwriteSpec/noPerms"
                   ,"test/HPath/IO/copyDirRecursiveSpec/noPerms"
                   ,"test/HPath/IO/copyDirRecursiveOverwriteSpec/noPerms"
                   ,"test/HPath/IO/createDirSpec/noPerms"
                   ,"test/HPath/IO/createRegularFileSpec/noPerms"
                   ,"test/HPath/IO/renameFileSpec/noPerms"
                   ,"test/HPath/IO/moveFileSpec/noPerms"
                   ,"test/HPath/IO/moveFileOverwriteSpec/noPerms"
                   ,"test/HPath/IO/recreateSymlinkSpec/noPerms"
                   ,"test/HPath/IO/getFileTypeSpec/noPerms"
                   ,"test/HPath/IO/getDirsFilesSpec/noPerms"
                   ,"test/HPath/IO/deleteFileSpec/noPerms"
                   ]
    fixPermissions = do
      sequence_ $ fmap noWritableDirPerms noWriteDirs
      sequence_ $ fmap noPerms noPermsDirs
    revertPermissions = do
      sequence_ $ fmap normalDirPerms noWriteDirs
      sequence_ $ fmap normalDirPerms noPermsDirs


