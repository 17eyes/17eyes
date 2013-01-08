{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

--
-- Copyright (c) 2012 by Tomasz Dudziak, Mateusz Kocielski
-- www.17eyes.com, hello@17eyes.com
--

module SQLUtils where

import Database.SQLite3
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

sqlDataToNum :: Num a => SQLData -> a
sqlDataToNum (SQLInteger x) = fromIntegral x
sqlDataToNum other = fromIntegral ((read $ fromSql other) :: Integer)

class SQLDataConv a where
  toSql :: a -> SQLData
  fromSql :: SQLData -> a

instance SQLDataConv Text.Text where
  toSql = SQLText

  fromSql (SQLText txt) = txt
  fromSql _ = error "Incompatible SQLData"

instance SQLDataConv String where
  toSql = toSql . Text.pack
  fromSql = Text.unpack . fromSql

instance SQLDataConv Int where
  toSql = SQLInteger . fromIntegral
  fromSql x = sqlDataToNum x

instance SQLDataConv Integer where
  toSql = SQLInteger . fromIntegral
  fromSql x = sqlDataToNum x

instance SQLDataConv a => SQLDataConv (Maybe a) where
  toSql Nothing = SQLNull
  toSql (Just x) = toSql x

  fromSql (SQLNull) = Nothing
  fromSql x = Just (fromSql x)

instance SQLDataConv BS.ByteString where
  toSql = SQLBlob

  fromSql (SQLBlob bs) = bs
  fromSql _ = error "Incompatible SQLData"

instance SQLDataConv BSL.ByteString where
  toSql = toSql . BS.concat . BSL.toChunks

  fromSql x = BSL.fromChunks [fromSql x]

run :: Database -> String -> [SQLData] -> IO ()
run db query param = do
  q <- prepare db $ Text.pack query
  bind q param
  step q
  finalize q

quickQuery :: Database -> String -> [SQLData] -> IO [[SQLData]]
quickQuery db query param = do
  q <- prepare db $ Text.pack query
  bind q param
  results <- fetchResults q
  finalize q
  return results
 where
   fetchResults q = do
     sr <- step q
     case sr of
       Done -> return []
       Row -> do
         cols <- columns q
         rest <- fetchResults q
         return (cols:rest)

execAndFinalize :: Statement -> IO ()
execAndFinalize stmt = do
  fetchAll
  finalize stmt
 where
   fetchAll = do
     sr <- step stmt
     case sr of
       Row -> fetchAll
       Done -> return ()
