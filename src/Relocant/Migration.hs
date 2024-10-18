{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Relocant.Migration
  ( Migration(..)
  , createTable
  , loadAll
  , loadByID
  ) where

import "crypton" Crypto.Hash (Digest, SHA1, digestFromByteString)
import Data.ByteString (ByteString)
import Data.Maybe (listToMaybe)
import Data.Time (ZonedTime, zonedTimeToUTC)
import Database.PostgreSQL.Simple qualified as DB
import Database.PostgreSQL.Simple.FromField qualified as DB (FromField)
import Database.PostgreSQL.Simple.FromRow qualified as DB (RowParser, field)
import Database.PostgreSQL.Simple.SqlQQ qualified as DB (sql)
import Prelude hiding (id, readFile)

import Relocant.Migration.ID qualified as Migration (ID)


data Migration = Migration
  { id        :: Migration.ID
  , name      :: Name
  , bytes     :: ByteString
  , sha1      :: Digest SHA1
  , appliedAt :: At
  } deriving (Show, Eq)

newtype Name = Name String
    deriving (Show, Eq, DB.FromField)

newtype At = At ZonedTime
    deriving (Show, DB.FromField)

instance Eq At where
  At x == At y =
    zonedTimeToUTC x == zonedTimeToUTC y

createTable :: DB.Connection -> IO ()
createTable conn = do
  _ <- DB.execute_ conn [DB.sql|
    SET client_min_messages TO 'warning'
  |]
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

loadAll :: DB.Connection -> IO [Migration]
loadAll conn = do
  createTable conn
  DB.queryWith_ migrationP conn [DB.sql|
    SELECT id
         , name
         , bytes
         , sha1
         , applied_at
      FROM migration
  ORDER BY id
  |]

loadByID :: Migration.ID -> DB.Connection -> IO (Maybe Migration)
loadByID id conn = do
  ms <- DB.queryWith migrationP conn [DB.sql|
    SELECT id
         , name
         , bytes
         , sha1
         , applied_at
      FROM migration
     WHERE id = ?
  |] (DB.Only id)
  pure (listToMaybe ms)

migrationP :: DB.RowParser Migration
migrationP = do
  id <- DB.field
  name <- DB.field
  bytes <- DB.field
  DB.Binary bs <- DB.field
  let
    Just sha1 =
      digestFromByteString @_ @ByteString bs
  appliedAt <- DB.field
  pure Migration {..}
