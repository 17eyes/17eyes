module Codebase(Codebase, scanCodebase, codebasePaths) where

import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory
import System.FilePath
import Control.Applicative((<$>))
import Control.Monad(filterM)

-- XXX: No place to remove this query ;[
createCodebaseDatabaseQuery :: String
createCodebaseDatabaseQuery = "" ++
  " CREATE TABLE resource ( " ++
  "     id   INTEGER PRIMARY KEY ASC AUTOINCREMENT," ++
  "     hash TEXT NOT NULL UNIQUE," ++
  "     utime INTEGER NOT NULL DEFAULT (date('now'))," ++
  "     cfg  BLOB" ++
  "  );" ++
  "  CREATE UNIQUE INDEX hash ON resource(hash);" ++
  "" ++
  "  CREATE TABLE file (" ++
  "      name TEXT UNIQUE," ++
  "      resource_id INTEGER REFERENCES resource(id) ON DELETE CASCADE" ++
  "          ON UPDATE CASCADE" ++
  "  );" ++
  "" ++
  "  CREATE UNIQUE INDEX file_name ON file(name);" ++
  "" ++
  "  CREATE TABLE function (" ++
  "      name TEXT," ++
  "      resource_id INTEGER REFERENCES resource(id) ON DELETE CASCADE" ++
  "          ON UPDATE CASCADE," ++
  "      cfg  BLOB" ++
  "  );" ++
  "" ++
  "  CREATE INDEX function_name ON file(name);" ++
  "" ++
  "  -- XXX: interfaces are also kept in class table (with proper type)" ++
  "" ++
  "  CREATE TABLE class (" ++
  "      type INTEGER NOT NULL, -- XXX additional check when dictionary is ready" ++
  "      name TEXT," ++
  "      resource_id INTEGER REFERENCES resource(id) ON DELETE CASCADE" ++
  "          ON UPDATE CASCADE  " ++
  "  );" ++
  "" ++
  "  CREATE INDEX class_name ON file(name);" ++
  "" ++
  "" ++
  "  CREATE TABLE method (" ++
  "      name TEXT," ++
  "      type INTEGER NOT NULL, -- XXX additional check when dictionary is ready" ++
  "      class_id INTEGER REFERENCES class(id) ON DELETE CASCADE" ++
  "          ON UPDATE CASCADE," ++
  "      cfg  BLOB" ++
  "  );" ++
  "" ++
  "  CREATE INDEX method_name ON file(name);"

data Codebase' = Codebase' String Connection

-- TODO: hashing, caching, etc.
data Codebase = MkCodebase [FilePath] deriving Show

createCodebase :: String -> IO Codebase'
createCodebase project = do
  let dbFile = "." ++ project ++ ".17b"
  conn <- connectSqlite3 dbFile
  tables <- getTables conn
  -- check if databse is initliazed XXX: check if database is not corrupted?
  if null tables
    then run conn createCodebaseDatabaseQuery []
    else return 0
  commit conn
  return $ Codebase' project conn

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
