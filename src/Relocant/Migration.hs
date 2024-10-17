{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
import Database.PostgreSQL.Simple.ToField qualified as DB (ToField)
import Database.PostgreSQL.Simple.FromRow qualified as DB (RowParser, field)
import Database.PostgreSQL.Simple.SqlQQ qualified as DB (sql)
import Prelude hiding (id, readFile)


data Migration = Migration
  { id        :: ID
  , name      :: Name
  , bytes     :: ByteString
  , sha1      :: Digest SHA1
  , appliedAt :: At
  } deriving (Show, Eq)

newtype ID = ID String
    deriving (Show, Eq, DB.FromField, DB.ToField)

newtype Name = Name String
    deriving (Show, Eq, DB.FromField)

newtype At = At ZonedTime
    deriving (Show, DB.FromField)

instance Eq At where
  At x == At y =
    zonedTimeToUTC x == zonedTimeToUTC y

loadAll :: DB.Connection -> IO [Migration]
loadAll conn =
  DB.queryWith_ migrationP conn [DB.sql|
    SELECT id
         , name
         , bytes
         , sha1
         , applied_at
      FROM migration
  ORDER BY id
  |]

loadByID :: ID -> DB.Connection -> IO (Maybe Migration)
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
