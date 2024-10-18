{-# LANGUAGE QuasiQuotes #-}
module Relocant.DB
  ( connect
  , init
  ) where

import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple qualified as DB
import Database.PostgreSQL.Simple.SqlQQ qualified as DB (sql)
import Prelude hiding (init)


connect :: ByteString -> IO DB.Connection
connect connectionString = do
  conn <- DB.connectPostgreSQL connectionString
  _ <- DB.execute_ conn [DB.sql|
    SET client_min_messages TO 'warning'
  |]
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
