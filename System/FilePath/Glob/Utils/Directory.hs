{-# LANGUAGE CPP #-}

module System.FilePath.Glob.Utils.Directory (isDirectory) where

-- | Utilities for directory and symlink handling in globbing.

import System.Directory (doesDirectoryExist)
#if !mingw32_HOST_OS
import System.Directory (pathIsSymbolicLink)
import System.FilePath.Glob.Utils.IO (catchIO)
import qualified System.Posix.Files as Posix
#endif

-- | Check if a path is a directory, optionally following symlinks.

isDirectory :: Bool -> FilePath -> IO Bool
#if mingw32_HOST_OS
isDirectory _ = doesDirectoryExist
#else
isDirectory followSymlinks path = do
  isSym <- pathIsSymbolicLink path
  if isSym
    then if followSymlinks
      then do
        statusResult <- (Just <$> Posix.getFileStatus path) `catchIO` (\_ -> return Nothing)
        case statusResult of
          Just status -> return (Posix.isDirectory status)
          Nothing -> return False
      else return False
    else doesDirectoryExist path
#endif
