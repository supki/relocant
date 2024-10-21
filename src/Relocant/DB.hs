{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
module Relocant.DB
  ( ConnectionString
  , Table
  , defaultTable
  , connect
  , init
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
import Prelude hiding (init)

import Relocant.DB.Table (Table, defaultTable)


newtype ConnectionString = ConnectionString ByteString
    deriving (Show, Eq, IsString)

connect :: ConnectionString -> Table -> IO DB.Connection
connect (ConnectionString str) table = do
  conn <- DB.connectPostgreSQL str
  init conn table
  pure conn

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

withLock :: Table -> DB.Connection -> IO a -> IO a
withLock table conn m =
  bracket_ (lock table conn) (unlock table conn) m

withTryLock :: Table -> DB.Connection -> (Bool -> IO a) -> IO a
withTryLock table conn m =
  bracket (tryLock table conn) release m
 where
  release locked =
    when locked $ do
      _ <- unlock table conn
      pure ()

lock :: Table -> DB.Connection -> IO ()
lock table conn = do
  [()] <- DB.queryWith DB.field conn [DB.sql|
    SELECT pg_advisory_lock(hashtext('?'))
  |] (DB.Only table)
  pure ()

tryLock :: Table -> DB.Connection -> IO Bool
tryLock table conn = do
  [locked] <- DB.queryWith DB.field conn [DB.sql|
    SELECT pg_try_advisory_lock(hashtext('?'))
  |] (DB.Only table)
  pure locked

unlock :: Table -> DB.Connection -> IO Bool
unlock table conn = do
  [unlocked] <- DB.queryWith DB.field conn [DB.sql|
    SELECT pg_advisory_unlock(hashtext('?'))
  |] (DB.Only table)
  pure unlocked
