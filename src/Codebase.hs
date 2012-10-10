--
-- Copyright (c) 2012 by Tomasz Dudziak, Mateusz Kocielski
-- www.17eyes.com, hello@17eyes.com
--

module Codebase(
    Codebase, createCodebase, updateCodebase,
    resolveFile, resolveFunction, resolveConstant, resolveMethod,
    moduleFunctions
  ) where

import Crypto.Hash.SHA1
import qualified Data.Text as Text
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.Base64 as ByteString.Base64
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.List
import qualified Database.SQLite3 as SQLite3
import System.Directory
import System.FilePath
import Control.Applicative((<$>))
import Control.Monad
import Common
import SQLUtils
import Lang.Php.Ast
import Lang.Php.Cfg.Generation
import Lang.Php.Cfg.Serialization
import Lang.Php.Cfg.Types
import Text.Printf

import qualified Compiler.Hoopl as Hoopl
import Compiler.Hoopl(Graph,C)

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

data Codebase = Codebase String FilePath Connection

-- TODO: hashing, caching, etc.
data CodebasePath = MkCodebase [FilePath] deriving Show

-- XXX: poprzednie bindingi

type Connection = SQLite3.Database

run :: SQLite3.Database -> String -> [SQLite3.SQLData] -> IO ()
run db query param = do
  q <- SQLite3.prepare db query
  SQLite3.bind q param
  SQLite3.step q
  SQLite3.finalize q

quickQuery' :: SQLite3.Database -> String -> [SQLite3.SQLData] -> IO [[SQLite3.SQLData]]
quickQuery' db query param = do
  q <- SQLite3.prepare db query
  SQLite3.bind q param
  results <- fetchResults q
  SQLite3.finalize q
  return results
 where
   fetchResults q = do
     sr <- SQLite3.step q
     case sr of
       SQLite3.Done -> return []
       SQLite3.Row -> do
         cols <- SQLite3.columns q
         rest <- fetchResults q
         return (cols:rest)

execAndFinalize :: SQLite3.Statement -> IO ()
execAndFinalize stmt = do
  fetchAll
  SQLite3.finalize stmt
 where
   fetchAll = do
     sr <- SQLite3.step stmt
     case sr of
       SQLite3.Row -> fetchAll
       SQLite3.Done -> return ()

-- XXX: najlepiej bedzie rozszerzyc tu o nazwę pliku

toHex :: ByteString.ByteString -> String
toHex bytes = ByteString.unpack bytes >>= printf "%02X"

toChar :: ByteString.ByteString -> String
toChar bytes = ByteString.unpack bytes >>= printf "%c"

-- Ugly hack: encode the CFG in Base64
encode64 :: Binary a => a -> String
encode64 = toChar . ByteString.Base64.encode . ByteString.concat .
           ByteString.Lazy.toChunks . encode

decode64 :: Binary a => String -> a
decode64 x = decode $ ByteString.Lazy.fromChunks $
             [ByteString.Base64.decodeLenient $ ByteString.Char8.pack x]

resolveFile :: Codebase -> FilePath -> IO Cfg
resolveFile (Codebase _ _ conn) name = do
  r <- quickQuery'
         conn
         "SELECT cfg FROM file JOIN resource ON (resource_id = resource.id) WHERE name = ?"
         [toSql name]
  undefined
  return $ case r of
    [sqlData] -> error (show sqlData) -- TODO: decode64 (fromSql encoded_cfg)
    _ -> error "TODO"


moduleFunctions :: Codebase -> String -> IO [(String, Hoopl.Label)]
moduleFunctions (Codebase _ _ conn) file_name = do
  r <- quickQuery' conn
       "SELECT function.name, cfg \
       \ FROM file JOIN function ON function.resource_id = file.resource_id \
       \ WHERE file.name = ?"
       [toSql file_name]
  forM r $ \[s_name, s_cfg] -> do
    let (label, _) = decode64 $ fromSql s_cfg :: (Hoopl.Label, Graph InstrPos C C)
    return (fromSql s_name, label)

resolveFunction :: Codebase -> String -> IO [(Hoopl.Label, Graph InstrPos C C)]
resolveFunction (Codebase projectName path conn) name = do
  r <- quickQuery' conn "SELECT cfg FROM function WHERE name = ?"
                   [toSql name]
  return $ map (decode64 . fromSql . head) r

resolveConstant (Codebase projectName path conn) name = do
  r <- quickQuery' conn "SELECT cfg FROM constant WHERE name = ?" [name]
  return $ mapM (\[x] -> decode . ByteString.Lazy.fromChunks $
    [ByteString.Base64.decodeLenient . ByteString.Char8.pack $
    (fromSql x :: String)]) r

resolveMethod (Codebase projectName path conn) name = do
  r <- quickQuery' conn "SELECT cfg FROM method WHERE name = ?" [name]
  return $ mapM (\[x] -> decode . ByteString.Lazy.fromChunks $
    [ByteString.Base64.decodeLenient . ByteString.Char8.pack $
    (fromSql x :: String)]) r

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
    id <- addResourceToDatabase hash filePath (encode64 cfg)
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
        [toSql id, toSql name, toSql (encode64 (lab, cfg))]
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
    forM_ dbFiles (\[x] ->
      if fromSql x `elem` files
        then return ()
        else run conn "DELETE FROM file WHERE name = ?" [x] >> return ())

scanCodebase :: FilePath -> IO CodebasePath
scanCodebase path = do
  wd <- getCurrentDirectory
  let path' = normalise (wd </> path)
  MkCodebase <$> (findFiles f path')
 where
  f x = (takeExtension x) `elem` exts
  exts = [".php", ".php5"]

codebasePaths :: CodebasePath -> [FilePath]
codebasePaths (MkCodebase xs) = xs

findFiles :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findFiles f path = do
  xs <- getDirectoryContents path
  let fs = [ normalise (path </> x) | x <- xs, x !! 0 /= '.' ]
  fs' <- filterM doesFileExist fs
  let fs'' = filter f fs
  ds <- filterM doesDirectoryExist fs
  subdir_result <- mapM (findFiles f) ds
  return (fs'' ++ concat subdir_result)
