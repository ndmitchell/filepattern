module Test.IO(testIO) where

import Control.Monad
import System.FilePattern.Directory
import System.Info.Extra
import System.IO.Extra
import System.Directory.Extra
import System.Process.Extra
import Test.Util


testIO :: IO ()
testIO = testSymlink

-- Test for https://github.com/ndmitchell/filepattern/issues/5
testSymlink :: IO ()
testSymlink = unless isWindows $
    withTempDir $ \tdir ->
        withCurrentDirectory tdir $ do
            createDirectoryIfMissing True "dir"
            createDirectoryIfMissing True "dir/a"
            createDirectoryIfMissing True "dir/b"
            writeFile "dir/a/file" ""
            system_ "ln -s dir/a/file dir/b/file"
            result <- getDirectoryFiles "." ["**"]
            -- We don't see symlinks as answers (should that be changed?)
            let answer = ["dir/a/file"] -- "dir/b/file"
            assertBool (result == answer) "Symlink creation" [show result, show answer]
