{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Relocant.App
  ( run
  ) where

import Control.Monad (unless)
import Data.Foldable (for_, traverse_)
import Database.PostgreSQL.Simple qualified as DB (withTransaction)
import System.Exit (exitFailure)

import Relocant.App.Opts qualified as Opts
import Relocant.DB qualified as DB
import Relocant.Migration (Migration)
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
runUnapplied connectionString dir = do
  migrations <- loadAllAndMergeScripts connectionString dir
  traverse_ print migrations.unapplied

runApplied :: DB.ConnectionString -> IO ()
runApplied connectionString = do
  migrations <- loadAll connectionString
  traverse_ print migrations

runVerify :: DB.ConnectionString -> FilePath -> Bool -> IO ()
runVerify connectionString dir quiet = do
  migrations <- loadAllAndMergeScripts connectionString dir
  unless (Migration.Merge.converged migrations) $ do
    unless quiet $ do
      putStrLn "unrecorded:"
      traverse_ print migrations.unrecorded
      putStrLn "script missing:"
      traverse_ print migrations.scriptMissing
      putStrLn "content mismatch:"
      traverse_ print migrations.contentMismatch
      putStrLn "unapplied:"
      traverse_ print migrations.unapplied
    exitFailure

runApply :: DB.ConnectionString -> FilePath -> IO ()
runApply connectionString dir = do
  migrations <- loadAllAndMergeScripts connectionString dir
  unless (Migration.Merge.ready migrations)
    exitFailure
  for_ migrations.unapplied $ \migration -> do
    conn <- DB.connect connectionString
    DB.init conn
    DB.withTransaction conn $ do
      Script.run migration conn
      Script.recordApplied migration conn
  runVerify connectionString dir False

loadAll :: DB.ConnectionString -> IO [Migration]
loadAll connectionString = do
  conn <- DB.connect connectionString
  DB.init conn
  Migration.loadAll conn

-- FIXME: there has to be a better name for this function
loadAllAndMergeScripts :: DB.ConnectionString -> FilePath -> IO Migration.Merge.Result
loadAllAndMergeScripts connectionString dir = do
  migrations <- loadAll connectionString
  scripts <- Script.listDirectory dir
  pure (Migration.merge migrations scripts)
