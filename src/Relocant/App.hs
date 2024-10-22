{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Relocant.App
  ( run
  ) where

import Control.Monad (unless)
import Data.Foldable (for_, traverse_)
import Database.PostgreSQL.Simple qualified as DB (Connection, withTransaction)
import GHC.Records (HasField)
import Prelude hiding (id)
import System.Exit (die, exitFailure)

import Relocant.App.Env qualified as Env
import Relocant.App.Opts qualified as Opts
import Relocant.DB qualified as DB
import Relocant.Migration qualified as Migration
import Relocant.Migration.Interval (makeInterval_, zeroInterval)
import Relocant.Migration.Merge qualified as Migration (merge)
import Relocant.Migration.Merge qualified as Migration.Merge
import Relocant.Script qualified as Script


-- --format (tsv / json)
-- --with-content? we probably want to have separate commands for looking into a specific script/migration
--   unapplied --id ?
--   applied --id ?
-- actual logging

run :: IO ()
run = do
  env <- Env.parse
  cmd <- Opts.parse env
  case cmd of
    Opts.Unapplied opts ->
      runUnapplied opts
    Opts.Applied opts ->
      runApplied opts
    Opts.Verify opts ->
      runVerify opts
    Opts.Apply opts ->
      runApply opts
    Opts.Version version ->
      putStrLn version
    Opts.Internal internalCmd ->
      case internalCmd of
        Opts.DumpSchema opts ->
          runDumpSchema opts
        Opts.MarkApplied opts ->
          runMarkApplied opts
        Opts.Delete opts ->
          runDelete opts
        Opts.DeleteAll opts ->
          runDeleteAll opts

runUnapplied :: Opts.Unapplied -> IO ()
runUnapplied opts =
  withTryLock opts $ \conn -> do
    migrations <- loadAll opts.table conn opts.scripts
    traverse_ print migrations.unapplied

runApplied :: Opts.Applied -> IO ()
runApplied opts = do
  withTryLock opts $ \conn -> do
    migrations <- Migration.loadAll opts.table conn
    traverse_ print migrations

runVerify :: Opts.Verify -> IO ()
runVerify opts =
  withTryLock opts $ \conn ->
    verify opts.table conn opts.scripts opts.quiet

verify :: DB.Table -> DB.Connection -> FilePath -> Bool -> IO ()
verify table conn dir quiet = do
  migrations <- loadAll table conn dir
  unless (Migration.Merge.converged migrations) $ do
    unless quiet $ do
      unless (null migrations.unrecorded) $ do
        putStrLn "unrecorded:"
        traverse_ print migrations.unrecorded
      unless (null migrations.scriptMissing) $ do
        putStrLn "script missing:"
        traverse_ print migrations.scriptMissing
      unless (null migrations.contentMismatch) $ do
        putStrLn "content mismatch:"
        traverse_ print migrations.contentMismatch
      unless (null migrations.unapplied) $ do
        putStrLn "unapplied:"
        traverse_ print migrations.unapplied
    exitFailure

runApply :: Opts.Apply -> IO ()
runApply opts =
  withLock opts $ \conn -> do
    migrations <- loadAll opts.table conn opts.scripts
    unless (Migration.Merge.ready migrations)
      exitFailure
    for_ migrations.unapplied $ \script -> do
      DB.withTransaction conn $ do
        durationS <- makeInterval_ (Script.run script conn)
        Script.recordApplied opts.table script durationS conn
    verify opts.table conn opts.scripts False

runDumpSchema :: Opts.DumpSchema -> IO ()
runDumpSchema _opts =
  DB.dumpSchema

runMarkApplied :: Opts.MarkApplied -> IO ()
runMarkApplied opts =
  withTryLock opts $ \conn -> do
    script <- Script.readFile opts.script
    DB.withTransaction conn $ do
      _deleted <- Migration.deleteByID script.id opts.table conn
      Script.recordApplied opts.table script zeroInterval conn

runDelete :: Opts.Delete -> IO ()
runDelete opts =
  withTryLock opts $ \conn -> do
    deleted <- Migration.deleteByID opts.id opts.table conn
    unless deleted exitFailure

runDeleteAll :: Opts.DeleteAll -> IO ()
runDeleteAll opts =
  withTryLock opts $ \conn -> do
    Migration.deleteAll opts.table conn

withLock
  :: (HasField "table" cfg DB.Table, HasField "connString" cfg DB.ConnectionString)
  => cfg
  -> (DB.Connection -> IO b)
  -> IO b
withLock cfg m = do
  conn <- DB.connect cfg.connString cfg.table
  DB.withLock cfg.table conn $
    m conn

withTryLock
  :: (HasField "table" cfg DB.Table, HasField "connString" cfg DB.ConnectionString)
  => cfg
  -> (DB.Connection -> IO b) -> IO b
withTryLock cfg m = do
  conn <- DB.connect cfg.connString cfg.table
  DB.withTryLock cfg.table conn $ \locked -> do
    unless locked $
      die "couldn't lock the database, migration in progress?"
    m conn

loadAll :: DB.Table -> DB.Connection -> FilePath -> IO Migration.Merge.Result
loadAll table conn dir = do
  migrations <- Migration.loadAll table conn
  scripts <- Script.listDirectory dir
  pure (Migration.merge migrations scripts)
