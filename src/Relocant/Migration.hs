{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Relocant.Migration
  ( Migration(..)
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

import Relocant.DB qualified as DB (Table)
import Relocant.Migration.ID qualified as Migration (ID)
import Relocant.Migration.Name qualified as Migration (Name)
import Relocant.Migration.Interval qualified as Migration (Interval)


data Migration = Migration
  { id        :: Migration.ID
  , name      :: Migration.Name
  , bytes     :: ByteString
  , sha1      :: Digest SHA1
  , appliedAt :: At
  , durationS :: Migration.Interval
  } deriving (Show, Eq)

newtype At = At ZonedTime
    deriving (Show, DB.FromField)

instance Eq At where
  At x == At y =
    zonedTimeToUTC x == zonedTimeToUTC y

loadAll :: DB.Table -> DB.Connection -> IO [Migration]
loadAll table conn = do
  DB.queryWith migrationP conn [DB.sql|
    SELECT id
         , name
         , bytes
         , sha1
         , applied_at
         , EXTRACT(epoch FROM duration_s) :: INTEGER
      FROM ?
  ORDER BY id
  |] (DB.Only table)

loadByID :: DB.Table -> Migration.ID -> DB.Connection -> IO (Maybe Migration)
loadByID table id conn = do
  ms <- DB.queryWith migrationP conn [DB.sql|
    SELECT id
         , name
         , bytes
         , sha1
         , applied_at
         , EXTRACT(epoch FROM duration_s) :: INTEGER
      FROM ?
     WHERE id = ?
  |] (table, id)
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
  durationS <- DB.field
  pure Migration {..}
