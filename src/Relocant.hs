-- | A PostgreSQL migration library
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
  , Content
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
  , applyAll
  -- * Re-exports (postgresql-simple)
  --
  -- Generally, I'm against re-exports for convenience, but here I feel
  -- it really improves the user experience, so it goes:
  , Connection
  , withTransaction
  ) where

import Data.Foldable (for_)
import Database.PostgreSQL.Simple (Connection, withTransaction)

import Relocant.Applied (Applied, getApplied, getAppliedByID)
import Relocant.Applied qualified as Applied
import Relocant.Content (Content)
import Relocant.DB qualified as DB (ConnectionString, connect, withLock, withTryLock)
import Relocant.DB.Table qualified as DB (Table, defaultTable)
import Relocant.Duration (Duration)
import Relocant.ID (ID)
import Relocant.Merge qualified as Relocant (merge)
import Relocant.Merge qualified
import Relocant.Name (Name)
import Relocant.Script (Script(..), readScripts, readScript)
import Relocant.Script qualified as Script


-- | A convenience function that gets all applied migrations' records from
-- the DB and merges them together with the migration scripts from a given directory.
mergeAll :: FilePath -> DB.Table -> Connection -> IO Relocant.Merge.Merged
mergeAll dir table conn = do
  applieds <- getApplied table conn
  scripts <- readScripts dir
  pure (Relocant.Merge.merge applieds scripts)

-- | A convenience function that applies all unapplied migrations' scripts
-- and records their application in the DB. Each script is run in a separate
-- transaction.
applyAll :: Relocant.Merge.Merged -> DB.Table -> Connection -> IO ()
applyAll merged table conn =
  for_ merged.unapplied $ \script -> do
    withTransaction conn $ do
      applied <- Script.apply script conn
      Applied.record applied table conn
