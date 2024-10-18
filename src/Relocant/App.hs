{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Relocant.App
  ( run
  ) where

import Control.Monad (unless)
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

import qualified Meta_relocant as Meta


run :: IO ()
run = do
  cmd <- Opts.parse
  case cmd of
    Opts.Unapplied connectionString dir -> do
      migrations <-
        liftA2 Migration.merge
          (loadAll connectionString)
          (Script.listDirectory dir)
      traverse_ print migrations.unapplied
    Opts.Applied connectionString -> do
      migrations <- loadAll connectionString
      traverse_ print migrations
    Opts.Verify connectionString dir -> do
      migrations <-
        liftA2 Migration.merge
          (loadAll connectionString)
          (Script.listDirectory dir)
      unless (Migration.Merge.converged migrations) $ do
        putStrLn "unrecorded:"
        traverse_ print migrations.unrecorded
        putStrLn "script missing:"
        traverse_ print migrations.scriptMissing
        putStrLn "content mismatch:"
        traverse_ print migrations.contentMismatch
        putStrLn "unapplied:"
        traverse_ print migrations.unapplied
        exitFailure
    Opts.Version ->
      putStrLn Meta.version

loadAll :: ByteString -> IO [Migration]
loadAll connectionString = do
  conn <- DB.connect connectionString
  DB.init conn
  Migration.loadAll conn
