-- | Helper to create a temp directory tree without symlinks for testing
module Tests.Symlinks.NoSymlinksTree (withNoSymlinksTree) where

import System.IO.Temp (withTempDirectory)
import System.Directory (createDirectory)
import System.FilePath ((</>))
import Prelude (FilePath, IO, ($), (.), return, writeFile)

-- | Creates a temp directory with a nested structure and no symlinks.
-- Calls the action with the root directory.
withNoSymlinksTree :: (FilePath -> IO a) -> IO a
withNoSymlinksTree action =
  withTempDirectory "." "glob-nosymlink-test" $ \tmp -> do
    let d1 = tmp </> "dir1"
    let d2 = d1 </> "dir2"
    let d3 = tmp </> "dir3"
    createDirectory d1
    createDirectory d2
    createDirectory d3
    writeFile (d1 </> "a.txt") "a"
    writeFile (d2 </> "b.txt") "b"
    writeFile (d3 </> "c.txt") "c"
    action tmp
