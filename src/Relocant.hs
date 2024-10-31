module Relocant
  ( Script(..)
  , readScripts
  , readScript

  , DB.ConnectionString
  , DB.connect
  , DB.withLock
  , DB.withTryLock

  , Applied(..)
  , ID
  , Name
  , Duration
  , DB.Table
  , DB.defaultTable
  , getApplied
  , getAppliedByID

  , Relocant.merge
  , mergeAll
  , Relocant.Merge.Merged(..)
  , Relocant.Merge.ContentMismatch(..)
  , Relocant.Merge.canApply
  , Relocant.Merge.converged

  , Script.apply
  , Applied.record
  -- * Re-exports (postgresql-simple)
  --
  -- Generally, I'm against re-exports for convenience, but here I feel
  -- it really improves the user experience, so it goes:
  , Connection
  , withTransaction
  ) where

import Database.PostgreSQL.Simple (Connection, withTransaction)

import Relocant.Applied (Applied, getApplied, getAppliedByID)
import Relocant.Applied qualified as Applied
import Relocant.DB qualified as DB (ConnectionString, connect, withLock, withTryLock)
import Relocant.DB.Table qualified as DB (Table, defaultTable)
import Relocant.Duration (Duration)
import Relocant.ID (ID)
import Relocant.Merge qualified as Relocant (merge)
import Relocant.Merge qualified
import Relocant.Name (Name)
import Relocant.Script (Script(..), readScripts, readScript)
import Relocant.Script qualified as Script


mergeAll :: DB.Table -> Connection -> FilePath -> IO Relocant.Merge.Merged
mergeAll table conn dir = do
  migrations <- getApplied table conn
  scripts <- readScripts dir
  pure (Relocant.Merge.merge migrations scripts)
