{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- File created: 2008-10-10 13:40:35

module System.FilePath.Glob.Utils
   ( isLeft, fromLeft
   , increasingSeq
   , addToRange, inRange, overlap
   , dropLeadingZeroes
   , pathParts
   , nubOrd
   , partitionDL, tailDL
   , getRecursiveContents
   , getRecursiveContentsWithSymlinks
   ) where

import Control.Monad    (foldM)
import Data.List        ((\\))
import qualified Data.DList as DL
import Data.DList       (DList)
import qualified Data.Set as Set
import System.Directory (getDirectoryContents, pathIsSymbolicLink)
import System.FilePath  ((</>), isPathSeparator, dropDrive)
import System.FilePath.Glob.Internal
import System.FilePath.Glob.Utils.IO (catchIO)
import System.IO.Unsafe (unsafeInterleaveIO)

import System.FilePath.Glob.Utils.Directory (isDirectory)
import System.FilePath.Glob.Types (SymlinkBehavior(..))

#if mingw32_HOST_OS
import Data.Bits          ((.&.))
import System.Win32.Types (LPCTSTR, withTString)
import System.Win32.File  (FileAttributeOrFlag, fILE_ATTRIBUTE_DIRECTORY)
#endif


-- | Recursively list all files and directories under the given directory.
--
-- The traversal behavior for symlinks to directories is controlled by the
-- 'SymlinkBehavior' argument:
--
--   * 'FollowSymlinks': Symlinks to directories are followed, so the traversal
--     will recurse into them as if they were normal directories. This may result
--     in visiting the same file or directory multiple times if there are cycles.
--
--   * 'DoNotFollowSymlinks': Symlinks to directories are not followed; the
--     traversal will include the symlink itself in the result, but will not
--     recurse into it.
--
-- The result is a 'DList' of all files and directories (including the root),
-- in traversal order. The function is robust to IO errors (e.g., permission
-- denied), returning the directory itself if it cannot be read.
--
-- When there are no symlinks in the directory tree, both behaviors are
-- equivalent and produce the same result.
--
-- Note: On Windows, following symlinks is not supported and symlinked directories will not be traversed.
getRecursiveContentsWithSymlinks :: SymlinkBehavior -> FilePath -> IO (DList FilePath)
getRecursiveContentsWithSymlinks symlinkBehavior dir =
    flip catchIO (\_ -> return $ DL.singleton dir) $ do
         isSymDir <- pathIsSymbolicLink dir
         let followSymlinks = symlinkBehavior == FollowSymlinks
         if isSymDir && not followSymlinks
            then return $ DL.singleton dir
            else do
               raw <- getDirectoryContents dir
               let entries = map (dir </>) (raw \\ [".",".."])
               entryInfos <- mapM (\e -> do
                  isDir <- isDirectory followSymlinks e
                  return (e, isDir)) entries
               let (dirs,files) = ([e | (e,True) <- entryInfos], [e | (e,False) <- entryInfos])
               subs <- unsafeInterleaveIO . mapM (getRecursiveContentsWithSymlinks symlinkBehavior) $ dirs
               return $ DL.cons dir (DL.fromList files `DL.append` DL.concat subs)


inRange :: Ord a => (a,a) -> a -> Bool
inRange (a,b) c = c >= a && c <= b

-- returns Just (a range which covers both given ranges) or Nothing if they are
-- disjoint.
--
-- Assumes that the ranges are in the correct order, i.e. (fst x < snd x).
overlap :: Ord a => (a,a) -> (a,a) -> Maybe (a,a)
overlap (a,b) (c,d) =
   if b >= c
      then if b >= d
              then if a <= c
                      then Just (a,b)
                      else Just (c,b)
              else if a <= c
                      then Just (a,d)
                      else Just (c,d)
      else Nothing

addToRange :: (Ord a, Enum a) => (a,a) -> a -> Maybe (a,a)
addToRange (a,b) c
   | inRange (a,b) c = Just (a,b)
   | c == pred a     = Just (c,b)
   | c == succ b     = Just (a,c)
   | otherwise       = Nothing

-- fst of result is in reverse order so that:
--
-- If x = fst (increasingSeq (a:xs)), then
-- x == reverse [a .. head x]
increasingSeq :: (Eq a, Enum a) => [a] -> ([a],[a])
increasingSeq []     = ([],[])
increasingSeq (x:xs) = go [x] xs
 where
   go is       []     = (is,[])
   go is@(i:_) (y:ys) =
      if y == succ i
         then go (y:is) ys
         else (is, y:ys)
   go _ _ = error "Glob.increasingSeq :: internal error"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

fromLeft :: Either a b -> a
fromLeft (Left x) = x
fromLeft _        = error "fromLeft :: Right"

dropLeadingZeroes :: String -> String
dropLeadingZeroes s =
   let x = dropWhile (=='0') s
    in if null x then "0" else x

-- foo/bar/baz -> [foo/bar/baz,bar/baz,baz]
pathParts :: FilePath -> [FilePath]
pathParts p = p : let d = dropDrive p
                   in if null d || d == p
                         then     f d
                         else d : f d
 where
   f []  = []
   f (x:xs@(y:_)) | isPathSeparator x && isPathSeparator y = f xs
   f (x:xs) =
      if isPathSeparator x
         then xs : f xs
         else      f xs

#if mingw32_HOST_OS
#if defined(i386_HOST_ARCH)
foreign import stdcall unsafe "windows.h GetFileAttributesW" c_GetFileAttributes :: LPCTSTR -> IO FileAttributeOrFlag
#elif defined(x86_64_HOST_ARCH)
foreign import ccall unsafe "windows.h GetFileAttributesW" c_GetFileAttributes :: LPCTSTR -> IO FileAttributeOrFlag
#else
#error Unknown mingw32 arch
#endif
#endif

-- | Recursively list all files and directories, not following symlinks (legacy behavior).
getRecursiveContents :: FilePath -> IO (DList FilePath)
getRecursiveContents = getRecursiveContentsWithoutSymlinks


getRecursiveContentsWithoutSymlinks :: FilePath -> IO (DList FilePath)
getRecursiveContentsWithoutSymlinks dir =
   flip catchIO (\_ -> return $ DL.singleton dir) $ do

      raw <- getDirectoryContents dir

      let entries = map (dir </>) (raw \\ [".",".."])
      (dirs,files) <- partitionM doesDirectoryExistFast entries

      subs <- unsafeInterleaveIO . mapM getRecursiveContents $ dirs

      return$ DL.cons dir (DL.fromList files `DL.append` DL.concat subs)

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p_ = foldM (f p_) ([],[])
 where
   f p (ts,fs) x = p x >>= \b ->
      if b
         then return (x:ts, fs)
         else return (ts, x:fs)

partitionDL :: (a -> Bool) -> DList a -> (DList a, DList a)
partitionDL p_ = DL.foldr (f p_) (DL.empty,DL.empty)
 where
   f p x (ts,fs) =
      if p x
         then (DL.cons x ts, fs)
         else (ts, DL.cons x fs)

tailDL :: DList a -> DList a
#if MIN_VERSION_dlist(1,0,0)
tailDL = DL.fromList . DL.tail
#else
tailDL = DL.tail
#endif

nubOrd :: Ord a => [a] -> [a]
nubOrd = go Set.empty
 where
   go _ [] = []
   go set (x:xs) =
      if Set.member x set
         then go set xs
         else x : go (Set.insert x set) xs

