module Codebase(Codebase, scanCodebase, codebasePaths) where

import Crypto.Hash.SHA1
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory
import System.FilePath
import Control.Applicative((<$>))
import Control.Monad
import Common
import Lang.Php.Ast
import Lang.Php.Cfg.Generation
import Lang.Php.Cfg.Serialization
import Text.Printf

-- XXX: find better place to move this query
createCodebaseDatabaseQuery :: [String]
createCodebaseDatabaseQuery = [
  " CREATE TABLE resource ( " ++
  "     id   INTEGER PRIMARY KEY ASC AUTOINCREMENT," ++
  "     hash TEXT NOT NULL UNIQUE," ++
  "     utime INTEGER NOT NULL DEFAULT (strftime('%s','now'))," ++
  "     cfg  BLOB" ++
  "  );",
  "  CREATE UNIQUE INDEX hash ON resource(hash);",

  "  CREATE TABLE file (" ++
  "      name TEXT UNIQUE," ++
  "      resource_id INTEGER REFERENCES resource(id) ON DELETE CASCADE" ++
  "          ON UPDATE CASCADE" ++
  "  );",
  "  CREATE UNIQUE INDEX file_name ON file(name);",

  "  CREATE TABLE function (" ++
  "      name TEXT," ++
  "      resource_id INTEGER REFERENCES resource(id) ON DELETE CASCADE" ++
  "          ON UPDATE CASCADE," ++
  "      cfg  BLOB" ++
  "  );",
  "  CREATE INDEX function_name ON function(name);",

  -- XXX: interfaces are also kept in class table (with proper type)
  "  CREATE TABLE class (" ++
  -- XXX additional check when dictionary is ready
  "      type INTEGER NOT NULL," ++
  "      name TEXT," ++
  "      resource_id INTEGER REFERENCES resource(id) ON DELETE CASCADE" ++
  "          ON UPDATE CASCADE  " ++
  "  );",
  "  CREATE INDEX class_name ON class(name);",
  
  "  CREATE TABLE method (" ++
  "      name TEXT," ++
  -- XXX additional check when dictionary is ready
  "      type INTEGER NOT NULL," ++
  "      class_id INTEGER REFERENCES class(id) ON DELETE CASCADE" ++
  "          ON UPDATE CASCADE," ++
  "      cfg  BLOB" ++
  "  );",
  "  CREATE INDEX method_name ON method(name);",
  
  "  CREATE TABLE constant (" ++
  "      name TEXT," ++
  "      resource_id INTEGER REFERENCES resource(id) ON DELETE CASCADE" ++
  "          ON UPDATE CASCADE" ++
  "  );",
  "  CREATE INDEX constant_name ON constant(name);"]
 
data Codebase' = Codebase' String FilePath Connection

-- TODO: hashing, caching, etc.
data Codebase = MkCodebase [FilePath] deriving Show

createCodebase :: FilePath -> String -> IO Codebase'
createCodebase path projectName = do
  -- XXX: configuration
  let dbFile = "." ++ projectName ++ ".17b"
  conn <- connectSqlite3 dbFile
  runRaw conn "PRAGMA foreign_keys = ON;"
  tables <- getTables conn
  if null tables
    then mapM (\x -> run conn x []) createCodebaseDatabaseQuery
    else return []
  commit conn
  return $ Codebase' projectName path conn

updateCodebase (Codebase' projectName path conn) = do
  -- get files from the project's directory
  files <- codebasePaths <$> scanCodebase path
  timestamp <- getTimestamp
  -- remove not existing files
  removeDeadFiles files
  -- compute hashes
  hashes <- forM files $
    \fn -> liftM2 (,) (return fn) (toHex . hash <$> ByteString.readFile fn)
  -- update or add files
  forM hashes (\(fn, hash) -> do
    test <- existsResource hash
    if test
      then updateResource hash
      else addFileToCodeBase fn hash)
  -- remove dead resources
  removeDeadResources (timestamp-1)
  commit conn
  return (Codebase' projectName path conn) 

 where
  toHex :: ByteString.ByteString -> String
  toHex bytes = ByteString.unpack bytes >>= printf "%02X"

  addFileToCodeBase filePath hash = do
    -- Parse file
    source <- readFile filePath
    ast <- case runParser (parse :: Parser Ast) () filePath source of
      (Right ast) -> return ast
      -- XXX: error handler
      (Left err)  -> undefined
    -- CFG
    -- cfg <- return $ concatMap show $ runGMonad $ ByteString.unpack .
    -- ByteString.concat . ByteString.Lazy.toChunks . encode <$> (toCfg ast)
    cfg <- return ""
    id <- addResourceToDatabase hash filePath cfg
    addFileToDatabase filePath id
    -- add function / methods etc.
    commit conn
    return ()

  addFileToDatabase filePath id = do
    test <- existsFile filePath
    if test
      then run conn "UPDATE file SET resource_id = ? WHERE name = ?"
        [toSql filePath]
      else run conn "INSERT INTO file (resource_id, name) VALUES (?, ?);"
        [toSql id, toSql filePath]

  getTimestamp = do
    [[timestamp]] <- quickQuery' conn "SELECT strftime('%s','now');" []
    return (fromSql timestamp) :: IO Integer

  addResourceToDatabase hash filePath cfg = do
    putStrLn filePath >> return ()
    c <- run conn "INSERT INTO resource (hash, cfg) VALUES (?, ?);"
      [toSql hash, toSql cfg]
    [[id]] <- quickQuery' conn "SELECT last_insert_rowid();" []
    return (fromSql id) :: IO Integer
 
  updateResource hash = 
    run conn
      "UPDATE resource SET utime = strftime('%s','now') WHERE hash = ?;"
        [toSql hash]
      >> return ()

  existsResource hash = do
    c <- quickQuery' conn
      "SELECT 0 FROM resource WHERE hash = ?;" [toSql hash]
    return $ not . null $ c

  existsFile file = do
    c <- quickQuery' conn
      "SELECT 0 FROM file WHERE hash = ?;" [toSql file]
    return $ not . null $ c

  removeDeadResources timestamp = do
    run conn
      "DELETE FROM resource WHERE utime <= ?;" [toSql timestamp]

  removeDeadFiles files = do
    dbFiles <- quickQuery' conn "SELECT name FROM file" []
    forM_ dbFiles (\x -> 
      if fromSql (head x) `elem` files
        then return ()
        else run conn "DELETE FROM file WHERE name = ?" x >> return ())

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
