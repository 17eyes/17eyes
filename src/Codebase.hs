--
-- Copyright (c) 2012 by Tomasz Dudziak, Mateusz Kocielski
-- www.17eyes.com, hello@17eyes.com
--

module Codebase(
    Codebase, createCodebase, updateCodebase,
    resolveFile, resolveFunction, resolveConstant, resolveMethod,
    moduleFunctions
  ) where

import Data.List
import Control.Monad
import System.FilePath
import System.Directory
import Crypto.Hash.SHA1
import Text.Printf
import Control.Applicative((<$>))

import qualified Data.Text as Text
import qualified Data.ByteString as ByteString
import qualified Database.SQLite3 as SQLite3
import qualified Compiler.Hoopl as Hoopl
import Compiler.Hoopl(Graph,C)

import Common
import SQLUtils
import Lang.Php.Ast
import Lang.Php.Cfg.Generation
import Lang.Php.Cfg.Serialization
import Lang.Php.Cfg.Types

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

  "  CREATE TABLE class (" ++
  "      id INTEGER PRIMARY KEY ASC AUTOINCREMENT," ++
  "      type INTEGER NOT NULL CHECK (type IN ("++
         (show classNormal)++","++(show classInterface)++"))," ++
  "      name TEXT," ++
  "      resource_id INTEGER REFERENCES resource(id) ON DELETE CASCADE" ++
  "          ON UPDATE CASCADE  " ++
  "  );",
  "  CREATE INDEX class_name ON class(name);",

  "  CREATE TABLE method (" ++
  "      name TEXT," ++
  "      type INTEGER NOT NULL CHECK (type IN ("++
        (show methodNormal)++","++(show methodStatic) ++")),"++
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

data Codebase = Codebase String FilePath SQLite3.Database

toHex :: ByteString.ByteString -> String
toHex bytes = ByteString.unpack bytes >>= printf "%02X"

toChar :: ByteString.ByteString -> String
toChar bytes = ByteString.unpack bytes >>= printf "%c"

resolveFile :: Codebase -> FilePath -> IO [(String, Cfg)]
resolveFile (Codebase _ _ conn) name = do
  r <- quickQuery
         conn
         "SELECT name, cfg FROM file JOIN resource ON (resource_id = resource.id) WHERE name LIKE ? OR name = ?"
         [toSql ("%/" ++ name), toSql name]
  return $ map (\[sname, scfg] -> (fromSql sname, decode $ fromSql scfg)) r

moduleFunctions :: Codebase -> String -> IO [(String, Hoopl.Label)]
moduleFunctions (Codebase _ _ conn) file_name = do
  r <- quickQuery conn
       "SELECT function.name, cfg \
       \ FROM file JOIN function ON function.resource_id = file.resource_id \
       \ WHERE file.name = ?"
       [toSql file_name]
  forM r $ \[s_name, s_cfg] -> do
    let (label, _) = decode $ fromSql s_cfg :: (Hoopl.Label, Graph InstrPos C C)
    return (fromSql s_name, label)

resolveFunction :: Codebase -> String -> IO [(Hoopl.Label, Graph InstrPos C C)]
resolveFunction (Codebase projectName path conn) name = do
  r <- quickQuery conn "SELECT cfg FROM function WHERE name = ?"
                   [toSql name]
  return $ map (decode . fromSql . head) r

resolveConstant (Codebase projectName path conn) name = do
  r <- quickQuery conn "SELECT cfg FROM constant WHERE name = ?" [name]
  return $ map (decode . fromSql . head) r

resolveMethod :: Codebase -> String -> IO [Graph InstrPos C C]
resolveMethod (Codebase projectName path conn) name = do
  r <- quickQuery conn "SELECT cfg FROM method WHERE name = ?" [toSql name]
  return $ map (decode . fromSql . head) r

createCodebase :: FilePath -> String -> IO Codebase
createCodebase path projectName = do
  -- XXX: configuration
  let dbFile = "." ++ projectName ++ ".17b"
  db <- SQLite3.open dbFile
  SQLite3.prepare db "PRAGMA foreign_keys = ON;" >>= execAndFinalize

  -- count tables to check whether createCodebaseDatabaseQuery is needed
  stmt <- SQLite3.prepare db "SELECT COUNT(name) FROM sqlite_master;"
  SQLite3.Row <- SQLite3.step stmt
  (SQLite3.SQLInteger count) <- SQLite3.column stmt 0
  SQLite3.finalize stmt
  when (count == 0) $ forM_ createCodebaseDatabaseQuery $ \x ->
    SQLite3.prepare db x >>= execAndFinalize

  return (Codebase projectName path db)

updateCodebase (Codebase projectName path conn) = do
  -- get files from the project's directory
  files <- scanCodebase path
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
  return (Codebase projectName path conn)

 where
  addFileToCodeBase filePath hash = do
    -- Parse file
    source <- readFile filePath
    ast <- case runParser (parse :: Parser Ast) () filePath source of
      (Right ast) -> return ast
      -- XXX: error handler
      (Left err)  -> undefined
    let (cfg, decls) = runGMonad (toCfg ast)
    id <- addResourceToDatabase hash filePath cfg
    addFileToDatabase filePath id
    -- add function / methods etc.
    mapM (addDeclarableToDatabase id) decls
    return ()

  addFileToDatabase filePath id = do
    test <- existsFile filePath
    if test
      then run conn "UPDATE file SET resource_id = ? WHERE name = ?"
        [toSql id, toSql filePath]
      else run conn "INSERT INTO file (resource_id, name) VALUES (?, ?);"
        [toSql id, toSql filePath]

  addDeclarableToDatabase id (DFunction name lab cfg) = do
    run conn "INSERT INTO function (resource_id, name, cfg) VALUES (?, ?, ?)"
        [toSql id, toSql name, toSql (encode (lab, cfg))]
    return ()

  addDeclarableToDatabase id d@(DClass { dclsMethods = methods }) = do
    run conn "INSERT INTO class (type, resource_id, name) VALUES (?, ?, ?)"
        [toSql classNormal, toSql id, toSql (dclsName d)]
    [[class_id]] <- quickQuery conn "SELECT last_insert_rowid();" []
    forM methods $ \x@(_, name, _, _) ->
      run conn "INSERT INTO method (name, type, class_id, cfg) VALUES (?, ?, ?, ?)"
        [toSql name, toSql methodNormal, class_id, toSql (encode d)]
    return ()

  getTimestamp = do
    [[timestamp]] <- quickQuery conn "SELECT strftime('%s','now');" []
    return (fromSql timestamp) :: IO Integer

  addResourceToDatabase hash filePath cfg = do
    putStrLn filePath >> return ()
    c <- run conn "INSERT INTO resource (hash, cfg) VALUES (?, ?);"
      [toSql hash, toSql (encode cfg)]
    [[id]] <- quickQuery conn "SELECT last_insert_rowid();" []
    return (fromSql id) :: IO Integer

  updateResource hash =
    run conn
      "UPDATE resource SET utime = strftime('%s','now') WHERE hash = ?;"
        [toSql hash]
      >> return ()

  existsResource hash = do
    c <- quickQuery conn
      "SELECT 0 FROM resource WHERE hash = ?;" [toSql hash]
    return $ not . null $ c

  existsFile file = do
    c <- quickQuery conn
      "SELECT 0 FROM file WHERE name = ?;" [toSql file]
    return $ not . null $ c

  removeDeadResources timestamp = do
    run conn
      "DELETE FROM resource WHERE utime <= ?;" [toSql timestamp]

  removeDeadFiles files = do
    dbFiles <- quickQuery conn "SELECT name FROM file" []
    forM_ dbFiles (\[x] ->
      if fromSql x `elem` files
        then return ()
        else run conn "DELETE FROM file WHERE name = ?" [x] >> return ())

scanCodebase :: FilePath -> IO [FilePath]
scanCodebase path = do
  wd <- getCurrentDirectory
  let path' = normalise (wd </> path)
  findFiles f path'
 where
  f x = (takeExtension x) `elem` exts
  exts = [".php", ".php5"]

findFiles :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findFiles f path = do
  xs <- getDirectoryContents path
  let fs = [ normalise (path </> x) | x <- xs, x !! 0 /= '.' ]
  fs' <- filterM doesFileExist fs
  let fs'' = filter f fs
  ds <- filterM doesDirectoryExist fs
  subdir_result <- mapM (findFiles f) ds
  return (fs'' ++ concat subdir_result)
