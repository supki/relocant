{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Relocant.App
  ( run
  ) where

import Control.Monad (unless)
import Data.Foldable (for_, traverse_)
import Database.PostgreSQL.Simple qualified as DB (Connection, withTransaction)
import Prelude hiding (id)
import System.Exit (die, exitFailure)

import Relocant.App.Opts qualified as Opts
import Relocant.DB qualified as DB
import Relocant.Migration qualified as Migration
import Relocant.Migration.Interval (makeInterval_)
import Relocant.Migration.Merge qualified as Migration (merge)
import Relocant.Migration.Merge qualified as Migration.Merge
import Relocant.Script qualified as Script

import qualified Meta_relocant as Meta


run :: IO ()
run = do
  cmd <- Opts.parse
  case cmd of
    Opts.Unapplied connectionString table dir ->
      runUnapplied connectionString table dir
    Opts.Applied connectionString table ->
      runApplied connectionString table
    Opts.Verify connectionString table dir quiet ->
      runVerify connectionString table dir quiet
    Opts.Apply connectionString table dir ->
      runApply connectionString table dir
    Opts.Version ->
      putStrLn Meta.version
    Opts.Internal internalCmd ->
      case internalCmd of
        Opts.DumpSchema connectionString ->
          runDumpSchema connectionString
        Opts.MarkApplied connectionString table file ->
          runMarkApplied connectionString table file
        Opts.Delete connectionString table id ->
          runDelete connectionString table id
        Opts.DeleteAll connectionString table ->
          runDeleteAll connectionString table

runUnapplied :: DB.ConnectionString -> DB.Table -> FilePath -> IO ()
runUnapplied connectionString table dir =
  withTryLock connectionString table $ \conn -> do
    migrations <- loadAll table conn dir
    traverse_ print migrations.unapplied

runApplied :: DB.ConnectionString -> DB.Table -> IO ()
runApplied connectionString table = do
  withTryLock connectionString table $ \conn -> do
    migrations <- Migration.loadAll table conn
    traverse_ print migrations

runVerify :: DB.ConnectionString -> DB.Table -> FilePath -> Bool -> IO ()
runVerify connectionString table dir quiet =
  withTryLock connectionString table $ \conn ->
    verify table conn dir quiet

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

runApply :: DB.ConnectionString -> DB.Table -> FilePath -> IO ()
runApply connectionString table dir =
  withLock connectionString table $ \conn -> do
    migrations <- loadAll table conn dir
    unless (Migration.Merge.ready migrations)
      exitFailure
    for_ migrations.unapplied $ \script -> do
      DB.withTransaction conn $ do
        durationS <- makeInterval_ (Script.run script conn)
        Script.recordApplied table script durationS conn
    verify table conn dir False

runDumpSchema :: DB.ConnectionString -> IO ()
runDumpSchema _connectionString =
  DB.dumpSchema

runMarkApplied :: DB.ConnectionString -> DB.Table -> FilePath -> IO ()
runMarkApplied connectionString table path =
  withTryLock connectionString table $ \conn -> do
    script <- Script.readFile path
    durationS <- makeInterval_ (pure ())
    DB.withTransaction conn $ do
      _deleted <- Migration.deleteByID script.id table conn
      Script.recordApplied table script durationS conn

runDelete :: DB.ConnectionString -> DB.Table -> Migration.ID -> IO ()
runDelete connectionString table id =
  withTryLock connectionString table $ \conn -> do
    deleted <- Migration.deleteByID id table conn
    unless deleted exitFailure

runDeleteAll :: DB.ConnectionString -> DB.Table -> IO ()
runDeleteAll connectionString table =
  withTryLock connectionString table $ \conn -> do
    Migration.deleteAll table conn

withLock :: DB.ConnectionString -> DB.Table -> (DB.Connection -> IO b) -> IO b
withLock connectionString table m = do
  conn <- DB.connect connectionString table
  DB.withLock table conn $
    m conn

withTryLock :: DB.ConnectionString -> DB.Table -> (DB.Connection -> IO a) -> IO a
withTryLock connectionString table m = do
  conn <- DB.connect connectionString table
  DB.withTryLock table conn $ \locked -> do
    unless locked $
      die "couldn't lock the database, migration in progress?"
    m conn

loadAll :: DB.Table -> DB.Connection -> FilePath -> IO Migration.Merge.Result
loadAll table conn dir = do
  migrations <- Migration.loadAll table conn
  scripts <- Script.listDirectory dir
  pure (Migration.merge migrations scripts)
