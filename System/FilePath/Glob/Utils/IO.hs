-- | Minimal IO utility functions to avoid module cycles.
module System.FilePath.Glob.Utils.IO (catchIO) where

import qualified Control.Exception as E

catchIO :: IO a -> (IOError -> IO a) -> IO a
catchIO = E.catch
