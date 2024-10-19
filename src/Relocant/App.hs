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
    Opts.Unapplied connectionString dir ->
      runUnapplied connectionString dir
    Opts.Applied connectionString ->
      runApplied connectionString
    Opts.Verify connectionString dir quiet ->
      runVerify connectionString dir quiet
    Opts.Apply connectionString dir ->
      runApply connectionString dir
    Opts.Version ->
      putStrLn Meta.version

runUnapplied :: DB.ConnectionString -> FilePath -> IO ()
runUnapplied connectionString dir =
  withTryLock connectionString $ \conn -> do
    migrations <- loadAll conn dir
    traverse_ print migrations.unapplied

runApplied :: DB.ConnectionString -> IO ()
runApplied connectionString = do
  withTryLock connectionString $ \conn -> do
    migrations <- Migration.loadAll conn
    traverse_ print migrations

runVerify :: DB.ConnectionString -> FilePath -> Bool -> IO ()
runVerify connectionString dir quiet =
  withTryLock connectionString $ \conn ->
    verify conn dir quiet

verify :: DB.Connection -> FilePath -> Bool -> IO ()
verify conn dir quiet = do
  migrations <- loadAll conn dir
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

runApply :: DB.ConnectionString -> FilePath -> IO ()
runApply connectionString dir =
  withLock connectionString $ \conn -> do
    migrations <- loadAll conn dir
    unless (Migration.Merge.ready migrations)
      exitFailure
    for_ migrations.unapplied $ \migration -> do
      DB.withTransaction conn $ do
        Script.run migration conn
        Script.recordApplied migration conn
    verify conn dir False

withLock :: DB.ConnectionString -> (DB.Connection -> IO b) -> IO b
withLock connectionString m = do
  conn <- DB.connect connectionString
  DB.withLock conn $
    m conn

withTryLock :: DB.ConnectionString -> (DB.Connection -> IO a) -> IO a
withTryLock connectionString m = do
  conn <- DB.connect connectionString
  DB.withTryLock conn $ \locked -> do
    unless locked $
      die "couldn't lock the database, migration in progress?"
    m conn

loadAll :: DB.Connection -> FilePath -> IO Migration.Merge.Result
loadAll conn dir = do
  migrations <- Migration.loadAll conn
  scripts <- Script.listDirectory dir
  pure (Migration.merge migrations scripts)
