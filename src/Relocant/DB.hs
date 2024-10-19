{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
module Relocant.DB
  ( ConnectionString
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
import Prelude hiding (init)


newtype ConnectionString = ConnectionString ByteString
    deriving (Show, Eq, IsString)

connect :: ConnectionString -> IO DB.Connection
connect (ConnectionString str) = do
  conn <- DB.connectPostgreSQL str
  _ <- DB.execute_ conn [DB.sql|
    SET client_min_messages TO 'warning'
  |]
  init conn
  pure conn

init :: DB.Connection -> IO ()
init conn =
  createMigrationTable conn

createMigrationTable :: DB.Connection -> IO ()
createMigrationTable conn = do
  _ <- DB.execute_ conn [DB.sql|
    CREATE TABLE IF NOT EXISTS migration
    ( id         TEXT NOT NULL
    , name       TEXT NOT NULL
    , bytes      BYTEA NOT NULL
    , sha1       BYTEA NOT NULL
    , applied_at TIMESTAMPTZ NOT NULL
    , PRIMARY KEY (id)
    )
  |]
  pure ()

withLock :: DB.Connection -> IO a -> IO a
withLock conn m =
  bracket_ (lock conn) (unlock conn) m

withTryLock :: DB.Connection -> (Bool -> IO a) -> IO a
withTryLock conn m =
  bracket (tryLock conn) release m
 where
  release locked =
    when locked $ do
      _ <- unlock conn
      pure ()
lock :: DB.Connection -> IO ()
lock conn = do
  [()] <- DB.queryWith_ DB.field conn [DB.sql|
    SELECT pg_advisory_lock(hashtext('migration'))
  |]
  pure ()

tryLock :: DB.Connection -> IO Bool
tryLock conn = do
  [locked] <- DB.queryWith_ DB.field conn [DB.sql|
    SELECT pg_try_advisory_lock(hashtext('migration'))
  |]
  pure locked

unlock :: DB.Connection -> IO Bool
unlock conn = do
  [unlocked] <- DB.queryWith_ DB.field conn [DB.sql|
    SELECT pg_advisory_unlock(hashtext('migration'))
  |]
  pure unlocked
