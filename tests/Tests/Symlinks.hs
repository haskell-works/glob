{-# LANGUAGE OverloadedStrings #-}
module Tests.Symlinks (tests) where

import Data.List (sort)
import System.Directory
import System.FilePath
import System.FilePath.Glob.Base
import System.FilePath.Glob.Directory
import System.FilePath.Glob.Types (SymlinkBehavior(..))
import System.FilePath.Glob.Utils (getRecursiveContents, getRecursiveContentsWithSymlinks)
import System.IO.Temp (withTempDirectory)
import System.Info (os)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit.Base hiding (Test)
import Tests.Symlinks.NoSymlinksTree (withNoSymlinksTree)

import qualified Data.DList as DL

-- Helper to create a temp directory with a real dir and a symlink to it
withSymlinkedDir :: (FilePath -> FilePath -> IO a) -> IO a
withSymlinkedDir action =
  if os == "mingw32"
    then error "Symlink tests are skipped on Windows."
    else withTempDirectory "." "glob-symlink-test" $ \tmp -> do
      putStrLn $ "[test debug] tmp: " ++ tmp
      let realDir = tmp </> "real"
      let linkDir = tmp </> "link"
      createDirectory realDir
      writeFile (realDir </> "foo.txt") "foo"
      let relTarget = makeDirectoryRelative (takeDirectory linkDir) realDir
      createDirectoryLink relTarget linkDir
      action realDir linkDir
  where
    makeDirectoryRelative base target =
      let baseParts = splitDirectories base
          targetParts = splitDirectories target
          common = length $ takeWhile (uncurry (==)) $ zip baseParts targetParts
          up = replicate (length baseParts - common) ".."
          down = drop common targetParts
      in joinPath (up ++ down)

-- Test that globDirWith finds files in symlinked directories when followSymlinks is True
caseFollowSymlinks :: Assertion
caseFollowSymlinks =
  if os == "mingw32"
    then putStrLn "[skip] Symlink test skipped on Windows." >> assertBool "Symlink test skipped on Windows" True
    else withSymlinkedDir $ \realDir linkDir -> do
      let parent = takeDirectory realDir
      entries <- listDirectory parent
      mapM_ (\e -> pathIsSymbolicLink (parent </> e)) entries -- just to force evaluation, no output
      let pat = compile "**/foo.txt"
      resNoFollow <- globDirWith (GlobOptions matchDefault False DoNotFollowSymlinks) [pat] parent
      resFollow   <- globDirWith (GlobOptions matchDefault False FollowSymlinks)  [pat] parent
      let foundNoFollow = concat (fst resNoFollow)
          foundFollow   = concat (fst resFollow)
      -- Should only find the real file if not following symlinks
      assertBool "Should not find file via symlink if not following symlinks" $ (linkDir </> "foo.txt") `notElem` foundNoFollow
      -- Should find both real and symlinked file if following symlinks
      assertBool "Should find file via symlink if following symlinks" $ (linkDir </> "foo.txt") `elem` foundFollow

-- Test that getRecursiveContentsWithSymlinks and getRecursiveContents return the same results when there are no symlinks
caseNoSymlinksEquivalence :: Assertion
caseNoSymlinksEquivalence = withNoSymlinksTree $ \root -> do
  let norm = map (drop (length root + 1)) . DL.toList
  resNoSymlinks <- getRecursiveContents root
  resWithSymlinks <- getRecursiveContentsWithSymlinks FollowSymlinks root
  let normNoSymlinks = norm resNoSymlinks
      normWithSymlinks = norm resWithSymlinks
  assertEqual "Results should be identical when no symlinks are present" (sort normNoSymlinks) (sort normWithSymlinks)

-- Test that getRecursiveContentsWithSymlinks DoNotFollowSymlinks and FollowSymlinks return the same results when there are no symlinks
caseNoSymlinksEquivalenceBothBehaviors :: Assertion
caseNoSymlinksEquivalenceBothBehaviors = withNoSymlinksTree $ \root -> do
  let norm = map (drop (length root + 1)) . DL.toList
  resNoFollow <- getRecursiveContentsWithSymlinks DoNotFollowSymlinks root
  resFollow   <- getRecursiveContentsWithSymlinks FollowSymlinks root
  let normNoFollow = norm resNoFollow
      normFollow   = norm resFollow
  assertEqual "Results should be identical for both symlink behaviors when no symlinks are present" (sort normNoFollow) (sort normFollow)

tests :: Test
tests = testGroup "Symlinks"
  [ testCase "followSymlinks option" caseFollowSymlinks
  , testCase "getRecursiveContentsWithSymlinks == getRecursiveContents without symlinks" caseNoSymlinksEquivalence
  , testCase "getRecursiveContentsWithSymlinks DoNotFollowSymlinks == FollowSymlinks without symlinks" caseNoSymlinksEquivalenceBothBehaviors
  ]
