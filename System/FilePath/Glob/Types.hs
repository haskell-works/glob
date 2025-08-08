module System.FilePath.Glob.Types where

-- | How to handle symlinks to directories during globbing
data SymlinkBehavior = FollowSymlinks | DoNotFollowSymlinks
  deriving (Eq, Show)
