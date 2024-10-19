{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
module Relocant.DB
  ( ConnectionString
  , TableName
  , connect
  , withLock
  , withTryLock
  ) where

import Control.Exception (bracket, bracket_)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.String (IsString)
import Database.PostgreSQL.Simple qualified as DB
import Database.PostgreSQL.Simple.FromRow qualified as DB (field)
import Database.PostgreSQL.Simple.SqlQQ qualified as DB (sql)
import Database.PostgreSQL.Simple.ToField qualified as DB (ToField)
import Database.PostgreSQL.Simple.Types qualified as DB (QualifiedIdentifier)
import Prelude hiding (init)


newtype ConnectionString = ConnectionString ByteString
    deriving (Show, Eq, IsString)

newtype TableName = TableName DB.QualifiedIdentifier
    deriving (Show, Eq, IsString, DB.ToField)

connect :: ConnectionString -> TableName -> IO DB.Connection
connect (ConnectionString str) tableName = do
  conn <- DB.connectPostgreSQL str
  _ <- DB.execute_ conn [DB.sql|
    SET client_min_messages TO 'warning'
  |]
  init conn tableName
  pure conn

init :: DB.Connection -> TableName -> IO ()
init conn tableName =
  ensureMigrationTableExists conn tableName

ensureMigrationTableExists :: DB.Connection -> TableName -> IO ()
ensureMigrationTableExists conn tableName = do
  _ <- DB.execute conn [DB.sql|
    CREATE TABLE IF NOT EXISTS ?
    ( id         TEXT NOT NULL
    , name       TEXT NOT NULL
    , bytes      BYTEA NOT NULL
    , sha1       BYTEA NOT NULL
    , applied_at TIMESTAMPTZ NOT NULL
    , PRIMARY KEY (id)
    )
  |] (DB.Only tableName)
  pure ()

withLock :: TableName -> DB.Connection -> IO a -> IO a
withLock tableName conn m =
  bracket_ (lock tableName conn) (unlock tableName conn) m

withTryLock :: TableName -> DB.Connection -> (Bool -> IO a) -> IO a
withTryLock tableName conn m =
  bracket (tryLock tableName conn) release m
 where
  release locked =
    when locked $ do
      _ <- unlock tableName conn
      pure ()

lock :: TableName -> DB.Connection -> IO ()
lock tableName conn = do
  [()] <- DB.queryWith DB.field conn [DB.sql|
    SELECT pg_advisory_lock(hashtext('?'))
  |] (DB.Only tableName)
  pure ()

tryLock :: TableName -> DB.Connection -> IO Bool
tryLock tableName conn = do
  [locked] <- DB.queryWith DB.field conn [DB.sql|
    SELECT pg_try_advisory_lock(hashtext('?'))
  |] (DB.Only tableName)
  pure locked

unlock :: TableName -> DB.Connection -> IO Bool
unlock tableName conn = do
  [unlocked] <- DB.queryWith DB.field conn [DB.sql|
    SELECT pg_advisory_unlock(hashtext('?'))
  |] (DB.Only tableName)
  pure unlocked
