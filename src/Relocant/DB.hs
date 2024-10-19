{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
module Relocant.DB
  ( ConnectionString
  , Table
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

newtype Table = Table DB.QualifiedIdentifier
    deriving (Show, Eq, IsString, DB.ToField)

connect :: ConnectionString -> Table -> IO DB.Connection
connect (ConnectionString str) table = do
  conn <- DB.connectPostgreSQL str
  _ <- DB.execute_ conn [DB.sql|
    SET client_min_messages TO 'warning'
  |]
  init conn table
  pure conn

init :: DB.Connection -> Table -> IO ()
init conn table =
  ensureMigrationTableExists conn table

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
