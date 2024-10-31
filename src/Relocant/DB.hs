{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
-- | This module manages PostgreSQL connections and /relocant/'s DB schema.
module Relocant.DB
  ( ConnectionString
  , Table
  , defaultTable
  , connect
  , init
  , withLock
  , withTryLock
  , lock
  , tryLock
  , unlock
  , dumpSchema
  ) where

import Control.Exception (bracket, bracket_)
import Control.Monad (when)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.String (IsString)
import Data.Text.Encoding qualified as Text
import Database.PostgreSQL.Simple qualified as DB
import Database.PostgreSQL.Simple.FromRow qualified as DB (field)
import Database.PostgreSQL.Simple.SqlQQ qualified as DB (sql)
import Prelude hiding (init)
import System.Process (callProcess)

import Relocant.DB.Table (Table, defaultTable)


-- | PostgreSQL connection string. (This is a newtype over 'ByteString')
--
-- for syntax, see: https://hackage.haskell.org/package/postgresql-simple/docs/Database-PostgreSQL-Simple.html#v:connectPostgreSQL
newtype ConnectionString = ConnectionString ByteString
    deriving
      ( Show
      , Eq
      , IsString
      )

instance Aeson.ToJSON ConnectionString where
  toJSON (ConnectionString connString) =
    Aeson.toJSON (Text.decodeUtf8Lenient connString)

-- | Connect to the DB using the given 'ConnectionString' and
-- initialize the migrations table.
connect :: ConnectionString -> Table -> IO DB.Connection
connect (ConnectionString str) table = do
  conn <- DB.connectPostgreSQL str
  init conn table
  pure conn

-- | Initialize the migrations table.
init :: DB.Connection -> Table -> IO ()
init conn table = do
  setLessVerboseLogging conn
  ensureMigrationTableExists conn table

setLessVerboseLogging :: DB.Connection -> IO ()
setLessVerboseLogging conn = do
  _ <- DB.execute_ conn [DB.sql|
    SET client_min_messages TO 'warning'
  |]
  pure ()

ensureMigrationTableExists :: DB.Connection -> Table -> IO ()
ensureMigrationTableExists conn table = do
  _ <- DB.execute conn [DB.sql|
    CREATE TABLE IF NOT EXISTS ?
    ( id         TEXT NOT NULL
    , name       TEXT NOT NULL
    , bytes      BYTEA NOT NULL
    , sha1       BYTEA NOT NULL
    , applied_at TIMESTAMPTZ NOT NULL
    , duration_s INTERVAL NOT NULL
    , PRIMARY KEY (id)
    )
  |] (DB.Only table)
  pure ()

-- | Use /pg_advisory_{lock,unlock}/ to restrict access to
-- the migrations table. The given table is used to
-- generate the lock's ID, so that in the unlikely case where
-- you have multiple migrations tables in your database you can
-- lock them separately.
withLock :: Table -> DB.Connection -> IO a -> IO a
withLock table conn m =
  bracket_ (lock table conn) (unlock table conn) m

-- | A non-blocking version of 'withLock'. 'True' is passed
-- to the callback if the lock was successfully acquired, 'False' otherwise.
withTryLock :: Table -> DB.Connection -> (Bool -> IO a) -> IO a
withTryLock table conn m =
  bracket (tryLock table conn) release m
 where
  release locked =
    when locked $ do
      _ <- unlock table conn
      pure ()

-- | Use /pg_advisory_lock/ to lock the database, restricting
-- access to the migrations table. The given table is used to
-- generate the lock's ID, so that in the unlikely case where
-- you have multiple migrations tables in your database you can
-- lock them separately.
lock :: Table -> DB.Connection -> IO ()
lock table conn = do
  [()] <- DB.queryWith DB.field conn [DB.sql|
    SELECT pg_advisory_lock(hashtext('?'))
  |] (DB.Only table)
  pure ()

-- | A non-blocking version of 'lock'. Returns 'True' if the
-- lock was successfully acquired, 'False' otherwise.
tryLock :: Table -> DB.Connection -> IO Bool
tryLock table conn = do
  [locked] <- DB.queryWith DB.field conn [DB.sql|
    SELECT pg_try_advisory_lock(hashtext('?'))
  |] (DB.Only table)
  pure locked

-- | Use /pg_advisory_unlock/ to unlock the database.
unlock :: Table -> DB.Connection -> IO Bool
unlock table conn = do
  [unlocked] <- DB.queryWith DB.field conn [DB.sql|
    SELECT pg_advisory_unlock(hashtext('?'))
  |] (DB.Only table)
  pure unlocked

dumpSchema :: IO ()
dumpSchema =
  callProcess "pg_dump" ["--schema-only", "--no-owner", "--no-acl"]
