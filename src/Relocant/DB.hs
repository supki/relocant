{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
module Relocant.DB
  ( ConnectionString
  , connect
  , init
  ) where

import Data.ByteString (ByteString)
import Data.String (IsString)
import Database.PostgreSQL.Simple qualified as DB
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
