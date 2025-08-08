{-# LANGUAGE CPP #-}

module System.FilePath.Glob.Internal
   ( doesDirectoryExistFast
   ) where

#if mingw32_HOST_OS
import Data.Bits          ((.&.))
import System.Win32.Types (LPCTSTR, withTString)
import System.Win32.File  (FileAttributeOrFlag, fILE_ATTRIBUTE_DIRECTORY)
#else
import Foreign.C.String      (withCString)
import Foreign.Marshal.Alloc (allocaBytes)
import System.FilePath
   (isDrive, dropTrailingPathSeparator, addTrailingPathSeparator)
import System.Posix.Internals (sizeof_stat, lstat, s_isdir, st_mode)
#endif

-- Significantly speedier than System.Directory.doesDirectoryExistFast.
doesDirectoryExistFast :: FilePath -> IO Bool
#if mingw32_HOST_OS
-- This one allocates more memory since it has to do a UTF-16 conversion, but
-- that can't really be helped: the below version is locale-dependent.
doesDirectoryExistFast = flip withTString $ \s -> do
   a <- c_GetFileAttributes s
   return (a /= 0xffffffff && a.&.fILE_ATTRIBUTE_DIRECTORY /= 0)
#else
doesDirectoryExistFast s =
   allocaBytes sizeof_stat $ \p ->
      withCString
         (if isDrive s
             then addTrailingPathSeparator s
             else dropTrailingPathSeparator s)
         $ \c -> do
            st <- lstat c p
            if st == 0
               then fmap s_isdir (st_mode p)
               else return False
#endif
