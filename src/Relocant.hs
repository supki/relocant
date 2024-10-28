module Relocant
  ( Script(..)
  , readScripts

  , DB.ConnectionString
  , DB.connect
  , DB.withLock
  , DB.withTryLock

  , Applied(..)
  , Migration.ID
  , Migration.Name
  , DB.Table
  , DB.defaultTable
  , getApplied

  , Migration.merge
  , mergeAll
  , Migration.Merge.Merged(..)
  , Migration.Merge.ContentMismatch(..)
  , Migration.Merge.canApply
  , Migration.Merge.converged

  , Script.apply
  , Applied.record
  ) where

import Database.PostgreSQL.Simple (Connection)

import Relocant.DB qualified as DB (ConnectionString, connect, withLock, withTryLock)
import Relocant.DB.Table qualified as DB (Table, defaultTable)
import Relocant.Script (Script(..))
import Relocant.Script qualified as Script
import Relocant.Migration.Applied (Applied)
import Relocant.Migration.Applied qualified as Applied
import Relocant.Migration.ID qualified as Migration (ID)
import Relocant.Migration.Merge qualified as Migration (merge)
import Relocant.Migration.Merge qualified as Migration.Merge
import Relocant.Migration.Name qualified as Migration (Name)


readScripts :: FilePath -> IO [Script]
readScripts = Script.readAll

getApplied :: DB.Table -> Connection -> IO [Applied]
getApplied = Applied.selectAll

mergeAll :: DB.Table -> Connection -> FilePath -> IO Migration.Merge.Merged
mergeAll table conn dir = do
  migrations <- Relocant.getApplied table conn
  scripts <- Relocant.readScripts dir
  pure (Migration.Merge.merge migrations scripts)
