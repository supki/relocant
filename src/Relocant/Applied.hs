{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Relocant.Applied
  ( Applied(..)
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
import Relocant.At (At)
import Relocant.ID (ID)
import Relocant.Name (Name)
import Relocant.Duration (Duration)


data Applied = Applied
  { id        :: ID
  , name      :: Name
  , bytes     :: ByteString
  , sha1      :: Digest SHA1
  , appliedAt :: At
  , durationS :: Duration
  } deriving (Show, Eq)

instance Aeson.ToJSON Applied where
  toJSON a =
    Aeson.object
      [ "id" .= a.id
      , "name" .= a.name
      , "sha1" .= show a.sha1
      , "applied_at" .= a.appliedAt
      , "duration_s" .= a.durationS
      ]

selectAll :: DB.Table -> DB.Connection -> IO [Applied]
selectAll table conn = do
  DB.queryWith appliedP conn [DB.sql|
    SELECT id
         , name
         , bytes
         , sha1
         , applied_at
         , EXTRACT(epoch FROM duration_s) :: REAL
      FROM ?
  ORDER BY id
  |] (DB.Only table)

selectByID :: ID -> DB.Table -> DB.Connection -> IO (Maybe Applied)
selectByID id table conn = do
  ms <- DB.queryWith appliedP conn [DB.sql|
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

record :: Applied -> DB.Table -> DB.Connection -> IO ()
record a table conn = do
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
     , a.id
     , a.name
     , DB.Binary a.bytes
     , DB.Binary (convert @_ @ByteString a.sha1)
     , a.appliedAt
     , a.durationS
     )
  pure ()

deleteAll :: DB.Table -> DB.Connection -> IO ()
deleteAll table conn = do
  _rows <- DB.execute conn [DB.sql|
    TRUNCATE ?
  |] (DB.Only table)
  pure ()

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
  DB.Binary bs <- DB.field
  let
    Just sha1 =
      digestFromByteString @_ @ByteString bs
  appliedAt <- DB.field
  durationS <- DB.field
  pure Applied {..}
