module Codebase(Codebase, scanCodebase, codebasePaths) where

import System.Directory
import System.FilePath
import Control.Applicative((<$>))
import Control.Monad(filterM)

-- TODO: hashing, caching, etc.
data Codebase = MkCodebase [FilePath] deriving Show

scanCodebase :: FilePath -> IO Codebase
scanCodebase path = do
    wd <- getCurrentDirectory
    let path' = normalise (wd </> path)
    MkCodebase <$> (findFiles f path')
 where
    f x = (takeExtension x) `elem` exts
    exts = [".php", ".php5"]

codebasePaths :: Codebase -> [FilePath]
codebasePaths (MkCodebase xs) = xs

-- FIXME: ugly
findFiles :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findFiles f path = do
    xs <- getDirectoryContents path
    let fs = [ normalise (path </> x) | x <- xs, x !! 0 /= '.' ]
    fs' <- filterM doesFileExist fs
    let fs'' = filter f fs
    ds <- filterM doesDirectoryExist fs
    subdir_result <- mapM (findFiles f) ds
    return (fs'' ++ concat subdir_result)
