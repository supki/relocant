{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
-- | This module deals with applied migrations and their records in the DB.
module Relocant.Applied
  ( Applied(..)
  , getApplied
  , getAppliedByID
  , record
  , deleteAll
  , deleteByID
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson ((.=))
import Data.ByteString (ByteString)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple qualified as DB
import Database.PostgreSQL.Simple.FromRow qualified as DB (RowParser, field)
import Database.PostgreSQL.Simple.SqlQQ qualified as DB (sql)
import GHC.Records (HasField(getField))
import Prelude hiding (id, readFile)

import Relocant.DB qualified as DB (Table)
import Relocant.At (At)
import Relocant.Checksum (Checksum(..))
import Relocant.Content (Content(..))
import Relocant.ID (ID)
import Relocant.Name (Name)
import Relocant.Duration (Duration)


-- | An applied migration. Generally, it's created be either running a
-- migration 'Relocant.Script' with 'Relocant.apply' or getting the records
-- from the DB with 'getApplied' or 'getAppliedByID'.
data Applied = Applied
  { id        :: ID
  , name      :: Name
  , content   :: Content
  , appliedAt :: At
  , durationS :: Duration -- ^ how long it took to apply the migration script
  } deriving (Show, Eq)

-- | The content (as in actual bytes) is skipped.
instance Aeson.ToJSON Applied where
  toJSON a =
    Aeson.object
      [ "id" .= a.id
      , "name" .= a.name
      , "checksum" .= show a.checksum
      , "applied_at" .= a.appliedAt
      , "duration_s" .= a.durationS
      ]

instance HasField "bytes" Applied ByteString where
  getField s =
    s.content.bytes

instance HasField "checksum" Applied Checksum where
  getField s =
    s.content.checksum

-- | Retrieve all 'Applied' migrations' records from the DB.
getApplied :: DB.Table -> DB.Connection -> IO [Applied]
getApplied table conn = do
  DB.queryWith appliedP conn [DB.sql|
    SELECT id
         , name
         , bytes
         , checksum
         , applied_at
         , EXTRACT(epoch FROM duration_s) :: REAL
      FROM ?
  ORDER BY id
  |] (DB.Only table)

-- | Retrieve the specific 'Applied' migration's record from the DB.
getAppliedByID :: ID -> DB.Table -> DB.Connection -> IO (Maybe Applied)
getAppliedByID id table conn = do
  ms <- DB.queryWith appliedP conn [DB.sql|
    SELECT id
         , name
         , bytes
         , checksum
         , applied_at
         , EXTRACT(epoch FROM duration_s) :: REAL
      FROM ?
     WHERE id = ?
  |] (table, id)
  pure (listToMaybe ms)

-- | Record a successfully 'Applied' migration.
record :: Applied -> DB.Table -> DB.Connection -> IO ()
record a table conn = do
  1 <- DB.execute conn [DB.sql|
    INSERT INTO ?
              ( id
              , name
              , bytes
              , checksum
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
     , a.id
     , a.name
     , DB.Binary a.bytes
     , a.checksum
     , a.appliedAt
     , a.durationS
     )
  pure ()

-- | Delete all 'Applied' migrations' records from the DB.
deleteAll :: DB.Table -> DB.Connection -> IO ()
deleteAll table conn = do
  _rows <- DB.execute conn [DB.sql|
    TRUNCATE ?
  |] (DB.Only table)
  pure ()

-- | Delete the specific 'Applied' migration's record from the DB.
deleteByID :: ID -> DB.Table -> DB.Connection -> IO Bool
deleteByID id table conn = do
  rows <- DB.execute conn [DB.sql|
    DELETE FROM ?
          WHERE id = ?
  |] (table, id)
  pure (rows > 0)

appliedP :: DB.RowParser Applied
appliedP = do
  id <- DB.field
  name <- DB.field
  DB.Binary bytes <- DB.field
  checksum <- DB.field
  appliedAt <- DB.field
  durationS <- DB.field
  pure Applied
    { content = Content {..}
    , ..
    }
