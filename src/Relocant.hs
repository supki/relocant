module Relocant
  ( Script(..)
  , readScripts

  , DB.ConnectionString
  , DB.connect
  , DB.withLock
  , DB.withTryLock

  , Applied(..)
  , ID
  , Name
  , DB.Table
  , DB.defaultTable
  , getApplied

  , Relocant.merge
  , mergeAll
  , Relocant.Merge.Merged(..)
  , Relocant.Merge.ContentMismatch(..)
  , Relocant.Merge.canApply
  , Relocant.Merge.converged

  , Script.apply
  , Applied.record
  ) where

import Database.PostgreSQL.Simple (Connection)

import Relocant.Applied (Applied)
import Relocant.Applied qualified as Applied
import Relocant.DB qualified as DB (ConnectionString, connect, withLock, withTryLock)
import Relocant.DB.Table qualified as DB (Table, defaultTable)
import Relocant.ID (ID)
import Relocant.Merge qualified as Relocant (merge)
import Relocant.Merge qualified
import Relocant.Name (Name)
import Relocant.Script (Script(..))
import Relocant.Script qualified as Script


readScripts :: FilePath -> IO [Script]
readScripts = Script.readAll

getApplied :: DB.Table -> Connection -> IO [Applied]
getApplied = Applied.selectAll

mergeAll :: DB.Table -> Connection -> FilePath -> IO Relocant.Merge.Merged
mergeAll table conn dir = do
  migrations <- Relocant.getApplied table conn
  scripts <- Relocant.readScripts dir
  pure (Relocant.Merge.merge migrations scripts)
