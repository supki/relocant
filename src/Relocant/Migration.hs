{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Relocant.Migration
  ( Migration(..)
  , Migration.ID
  , selectAll
  , selectByID
  , record
  , deleteAll
  , deleteByID
  ) where

import "crypton" Crypto.Hash (Digest, SHA1, digestFromByteString)
import Data.Aeson qualified as Aeson
import Data.Aeson ((.=))
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple qualified as DB
import Database.PostgreSQL.Simple.FromRow qualified as DB (RowParser, field)
import Database.PostgreSQL.Simple.SqlQQ qualified as DB (sql)
import Prelude hiding (id, readFile)

import Relocant.DB qualified as DB (Table)
import Relocant.Migration.At qualified as Migration (At)
import Relocant.Migration.ID qualified as Migration (ID)
import Relocant.Migration.Name qualified as Migration (Name)
import Relocant.Migration.Interval qualified as Migration (Interval)


data Migration = Migration
  { id        :: Migration.ID
  , name      :: Migration.Name
  , bytes     :: ByteString
  , sha1      :: Digest SHA1
  , appliedAt :: Migration.At
  , durationS :: Migration.Interval
  } deriving (Show, Eq)

instance Aeson.ToJSON Migration where
  toJSON m =
    Aeson.object
      [ "id" .= m.id
      , "name" .= m.name
      , "sha1" .= show m.sha1
      , "applied_at" .= m.appliedAt
      , "duration_s" .= m.durationS
      ]

selectAll :: DB.Table -> DB.Connection -> IO [Migration]
selectAll table conn = do
  DB.queryWith migrationP conn [DB.sql|
    SELECT id
         , name
         , bytes
         , sha1
         , applied_at
         , EXTRACT(epoch FROM duration_s) :: REAL
      FROM ?
  ORDER BY id
  |] (DB.Only table)

selectByID :: Migration.ID -> DB.Table -> DB.Connection -> IO (Maybe Migration)
selectByID id table conn = do
  ms <- DB.queryWith migrationP conn [DB.sql|
    SELECT id
         , name
         , bytes
         , sha1
         , applied_at
         , EXTRACT(epoch FROM duration_s) :: REAL
      FROM ?
     WHERE id = ?
  |] (table, id)
  pure (listToMaybe ms)

record :: Migration -> DB.Table -> DB.Connection -> IO ()
record m table conn = do
  1 <- DB.execute conn [DB.sql|
    INSERT INTO ?
              ( id
              , name
              , bytes
              , sha1
              , applied_at
              , duration_s
              )
         SELECT ?
              , ?
              , ?
              , ?
              , ?
              , make_interval(secs := ?)
  |] ( table
     , m.id
     , m.name
     , DB.Binary m.bytes
     , DB.Binary (convert @_ @ByteString m.sha1)
     , m.appliedAt
     , m.durationS
     )
  pure ()

deleteAll :: DB.Table -> DB.Connection -> IO ()
deleteAll table conn = do
  _rows <- DB.execute conn [DB.sql|
    TRUNCATE ?
  |] (DB.Only table)
  pure ()

deleteByID :: Migration.ID -> DB.Table -> DB.Connection -> IO Bool
deleteByID id table conn = do
  rows <- DB.execute conn [DB.sql|
    DELETE FROM ?
          WHERE id = ?
  |] (table, id)
  pure (rows > 0)

migrationP :: DB.RowParser Migration
migrationP = do
  id <- DB.field
  name <- DB.field
  DB.Binary bytes <- DB.field
  DB.Binary bs <- DB.field
  let
    Just sha1 =
      digestFromByteString @_ @ByteString bs
  appliedAt <- DB.field
  durationS <- DB.field
  pure Migration {..}
