module Codebase(Codebase, scanCodebase, codebasePaths) where

import Crypto.Hash.SHA1
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.Base64 as ByteString.Base64
import qualified Data.ByteString.Char8 as ByteString.Char8
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
import Lang.Php.Cfg.Types
import Text.Printf

-- XXX: find better place to move this query
createCodebaseDatabaseQuery :: [String]
createCodebaseDatabaseQuery = [
  " CREATE TABLE resource ( " ++
  "     id   INTEGER PRIMARY KEY ASC AUTOINCREMENT," ++
  "     hash TEXT NOT NULL UNIQUE," ++
  "     utime INTEGER NOT NULL DEFAULT (strftime('%s','now'))," ++
  "     cfg  TEXT" ++
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
  "      id INTEGER PRIMARY KEY ASC AUTOINCREMENT," ++
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

-- constants for `type' columns
classNormal    = 0 :: Int
classInterface = 1 :: Int
methodNormal   = 0 :: Int
methodStatic   = 1 :: Int

data Codebase' = Codebase' String FilePath Connection

-- TODO: hashing, caching, etc.
data Codebase = MkCodebase [FilePath] deriving Show

-- XXX: najlepiej bedzie rozszerzyc tu o nazwę pliku

resolveFunction (Codebase' projectName path conn) name = do
  r <- quickQuery' conn "SELECT cfg FROM function WHERE name = ?" [toSql name]
  return $ mapM (\[x] -> decode . ByteString.Lazy.fromChunks $
    [ByteString.Base64.decodeLenient . ByteString.Char8.pack $
    (fromSql x :: String)]) r

resolveConstant (Codebase' projectName path conn) name = do
  r <- quickQuery' conn "SELECT cfg FROM constant WHERE name = ?" [toSql name]
  return $ mapM (\[x] -> decode . ByteString.Lazy.fromChunks $
    [ByteString.Base64.decodeLenient . ByteString.Char8.pack $
    (fromSql x :: String)]) r

resolveMethod (Codebase' projectName path conn) name = do
  r <- quickQuery' conn "SELECT cfg FROM method WHERE name = ?" [toSql name]
  return $ mapM (\[x] -> decode . ByteString.Lazy.fromChunks $
    [ByteString.Base64.decodeLenient . ByteString.Char8.pack $
    (fromSql x :: String)]) r

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

  toChar :: ByteString.ByteString -> String
  toChar bytes = ByteString.unpack bytes >>= printf "%c"

  -- Ugly hack: encode the CFG in Base64
  encode64 :: Binary a => a -> String
  encode64 = toChar . ByteString.Base64.encode . ByteString.concat .
              ByteString.Lazy.toChunks . encode

  addFileToCodeBase filePath hash = do
    -- Parse file
    source <- readFile filePath
    ast <- case runParser (parse :: Parser Ast) () filePath source of
      (Right ast) -> return ast
      -- XXX: error handler
      (Left err)  -> undefined
    let (cfg, decls) = runGMonad (toCfg ast)
    id <- addResourceToDatabase hash filePath (encode64 cfg)
    addFileToDatabase filePath id
    -- add function / methods etc.
    mapM (addDeclarableToDatabase id) decls
    commit conn
    return ()

  addFileToDatabase filePath id = do
    test <- existsFile filePath
    if test
      then run conn "UPDATE file SET resource_id = ? WHERE name = ?"
        [toSql filePath]
      else run conn "INSERT INTO file (resource_id, name) VALUES (?, ?);"
        [toSql id, toSql filePath]

  addDeclarableToDatabase id (DFunction name lab cfg) = do
    run conn "INSERT INTO function (resource_id, name, cfg) VALUES (?, ?, ?)"
        [toSql id, toSql name, toSql (encode64 cfg)]
    return ()

  addDeclarableToDatabase id d@(DClass { dclsMethods = methods }) = do
    run conn "INSERT INTO class (type, resource_id, name) VALUES (?, ?, ?)"
        [toSql classNormal, toSql id, toSql (dclsName d)]
    [[class_id]] <- quickQuery' conn "SELECT last_insert_rowid();" []
    forM methods $ \x@(_, name, _, _) ->
      run conn "INSERT INTO method (name, type, class_id, cfg) VALUES (?, ?, ?, ?)"
        [toSql name, toSql methodNormal, class_id, toSql (encode64 d)]
    return ()

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
      "SELECT 0 FROM file WHERE name = ?;" [toSql file]
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
