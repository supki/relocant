{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Relocant.App
  ( run
  ) where

import Control.Monad (unless)
import Data.Foldable (for_, traverse_)
import Database.PostgreSQL.Simple qualified as DB (Connection, withTransaction)
import System.Exit (die, exitFailure)

import Relocant.App.Opts qualified as Opts
import Relocant.DB qualified as DB
import Relocant.Migration qualified as Migration
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

runUnapplied :: DB.ConnectionString -> DB.TableName -> FilePath -> IO ()
runUnapplied connectionString table dir =
  withTryLock connectionString table $ \conn -> do
    migrations <- loadAll table conn dir
    traverse_ print migrations.unapplied

runApplied :: DB.ConnectionString -> DB.TableName -> IO ()
runApplied connectionString table = do
  withTryLock connectionString table $ \conn -> do
    migrations <- Migration.loadAll table conn
    traverse_ print migrations

runVerify :: DB.ConnectionString -> DB.TableName -> FilePath -> Bool -> IO ()
runVerify connectionString table dir quiet =
  withTryLock connectionString table $ \conn ->
    verify table conn dir quiet

verify :: DB.TableName -> DB.Connection -> FilePath -> Bool -> IO ()
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

runApply :: DB.ConnectionString -> DB.TableName -> FilePath -> IO ()
runApply connectionString table dir =
  withLock connectionString table $ \conn -> do
    migrations <- loadAll table conn dir
    unless (Migration.Merge.ready migrations)
      exitFailure
    for_ migrations.unapplied $ \script -> do
      DB.withTransaction conn $ do
        Script.run script conn
        Script.recordApplied script conn
    verify table conn dir False

withLock :: DB.ConnectionString -> DB.TableName -> (DB.Connection -> IO b) -> IO b
withLock connectionString tableName m = do
  conn <- DB.connect connectionString tableName
  DB.withLock tableName conn $
    m conn

withTryLock :: DB.ConnectionString -> DB.TableName -> (DB.Connection -> IO a) -> IO a
withTryLock connectionString tableName m = do
  conn <- DB.connect connectionString tableName
  DB.withTryLock tableName conn $ \locked -> do
    unless locked $
      die "couldn't lock the database, migration in progress?"
    m conn

loadAll :: DB.TableName -> DB.Connection -> FilePath -> IO Migration.Merge.Result
loadAll table conn dir = do
  migrations <- Migration.loadAll table conn
  scripts <- Script.listDirectory dir
  pure (Migration.merge migrations scripts)
