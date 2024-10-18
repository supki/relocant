{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Relocant.App
  ( run
  ) where

import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import System.Exit (exitFailure)

import Relocant.App.Opts qualified as Opts
import Relocant.DB qualified as DB
import Relocant.Migration (Migration)
import Relocant.Migration qualified as Migration
import Relocant.Migration.Merge qualified as Migration (merge)
import Relocant.Migration.Merge qualified as Migration.Merge
import Relocant.Script qualified as Script


run :: IO ()
run = do
  cmd <- Opts.parse
  case cmd of
    Opts.ListScripts dir -> do
      scripts <- Script.listDirectory dir
      traverse_ print scripts
    Opts.ListMigrations connectionString -> do
      migrations <- loadAll connectionString
      traverse_ print migrations
    Opts.DryRun connectionString dir -> do
      migrations <- loadAll connectionString
      scripts <- Script.listDirectory dir
      case Migration.merge migrations scripts of
        result
          | Migration.Merge.canApply result ->
            traverse_ print result.unapplied
          | otherwise -> do
            print result.unrecorded
            print result.scriptMissing
            print result.contentMismatch
            exitFailure

loadAll :: ByteString -> IO [Migration]
loadAll connectionString = do
  conn <- DB.connect connectionString
  DB.init conn
  Migration.loadAll conn
