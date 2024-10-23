{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Relocant.App
  ( run
  ) where

import Control.Monad (unless)
import Data.Aeson ((.=))
import Data.Foldable (for_, traverse_)
import Database.PostgreSQL.Simple qualified as DB (Connection, withTransaction)
import GHC.Records (HasField)
import Prelude hiding (id, log)
import System.Exit (exitFailure)

import Relocant.App.Env qualified as Env
import Relocant.App.Log (Log)
import Relocant.App.Log qualified as Log
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
  (cfg, cmd) <- Opts.parse env
  runCmd (Log.make cfg.minSeverity) cmd

runCmd :: Log -> Opts.Cmd -> IO ()
runCmd log = \case
  Opts.ListUnapplied opts -> do
    Log.debug log
      [ "action" .= ("list-unapplied" :: String)
      , "opts" .= opts
      ]
    runUnapplied log opts
  Opts.ListApplied opts -> do
    Log.debug log
      [ "action" .= ("list-applied" :: String)
      , "opts" .= opts
      ]
    runApplied log opts
  Opts.Verify opts -> do
    Log.debug log
      [ "action" .= ("verify" :: String)
      , "opts" .= opts
      ]
    runVerify log opts
  Opts.Apply opts -> do
    Log.debug log
      [ "action" .= ("apply" :: String)
      , "opts" .= opts
      ]
    runApply log opts
  Opts.Version version ->
    putStrLn version
  Opts.Internal internalCmd ->
    runInternalCmd log internalCmd

runInternalCmd :: Log -> Opts.InternalCmd -> IO ()
runInternalCmd log = \case
  Opts.DumpSchema opts -> do
    Log.debug log
      [ "action" .= ("dump-schema" :: String)
      , "opts" .= opts
      ]
    runDumpSchema opts
  Opts.MarkApplied opts -> do
    Log.debug log
      [ "action" .= ("mark-applied" :: String)
      , "opts" .= opts
      ]
    runMarkApplied log opts
  Opts.Delete opts -> do
    Log.debug log
      [ "action" .= ("delete" :: String)
      , "opts" .= opts
      ]
    runDelete log opts
  Opts.DeleteAll opts -> do
    Log.debug log
      [ "action" .= ("delete-all" :: String)
      , "opts" .= opts
      ]
    runDeleteAll log opts

runUnapplied :: Log -> Opts.Unapplied -> IO ()
runUnapplied log opts = do
  withTryLock log opts $ \conn -> do
    migrations <- loadAll opts.table conn opts.scripts
    traverse_ print migrations.unapplied

runApplied :: Log -> Opts.Applied -> IO ()
runApplied log opts = do
  withTryLock log opts $ \conn -> do
    migrations <- Migration.loadAll opts.table conn
    traverse_ print migrations

runVerify :: Log -> Opts.Verify -> IO ()
runVerify log opts = do
  withTryLock log opts $ \conn -> do
    verify log opts.table conn opts.scripts opts.quiet

runApply :: Log -> Opts.Apply -> IO ()
runApply log opts = do
  withLock log opts $ \conn -> do
    apply log opts.table conn opts.scripts
    verify log opts.table conn opts.scripts False

runDumpSchema :: Opts.DumpSchema -> IO ()
runDumpSchema _opts =
  DB.dumpSchema

runMarkApplied :: Log -> Opts.MarkApplied -> IO ()
runMarkApplied log opts = do
  withTryLock log opts $ \conn -> do
    script <- Script.readFile opts.script
    DB.withTransaction conn $ do
      Log.info log
        [ "action" .= ("mark-applied" :: String)
        , "delete" .= script.id
        ]
      _deleted <- Migration.deleteByID script.id opts.table conn
      Log.info log
        [ "action" .= ("mark-applied" :: String)
        , "record" .= script.id
        ]
      Script.recordApplied opts.table script zeroInterval conn

runDelete :: Log -> Opts.Delete -> IO ()
runDelete log opts = do
  withTryLock log opts $ \conn -> do
    deleted <- Migration.deleteByID opts.id opts.table conn
    unless deleted $ do
      Log.notice log
        [ "action" .= ("delete" :: String)
        , "result" .= ("couldn't delete the recorded migration, typo in the ID?" :: String)
        ]
      exitFailure

runDeleteAll :: Log -> Opts.DeleteAll -> IO ()
runDeleteAll log opts = do
  withTryLock log opts $ \conn -> do
    Migration.deleteAll opts.table conn

verify :: Log -> DB.Table -> DB.Connection -> FilePath -> Bool -> IO ()
verify log table conn dir quiet = do
  migrations <- loadAll table conn dir
  unless (Migration.Merge.converged migrations) $ do
    Log.debug log
      [ "action" .= ("apply" :: String)
      , "unrecorded" .= map (.id) migrations.unrecorded
      , "script-missing" .= map (.id) migrations.scriptMissing
      , "content-mismatch" .= map (\(s, m) -> (s.id, m.id)) migrations.contentMismatch
      , "unapplied" .= map (.id) migrations.unapplied
      ]
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

apply :: Log -> DB.Table -> DB.Connection -> FilePath -> IO ()
apply log table conn scripts = do
  migrations <- loadAll table conn scripts
  unless (Migration.Merge.ready migrations) $ do
    Log.error log
      [ "action" .= ("apply" :: String)
      , "result" .= ("not ready to apply the scripts, run `verify' first" :: String)
      ]
    exitFailure
  Log.info log
    [ "action" .= ("apply" :: String)
    , "migrations" .= map (.id) migrations.unapplied
    ]
  for_ migrations.unapplied $ \script -> do
    DB.withTransaction conn $ do
      Log.info log
        [ "action" .= ("apply" :: String)
        , "run" .= script.id
        ]
      durationS <- makeInterval_ (Script.run script conn)
      Log.info log
        [ "action" .= ("apply" :: String)
        , "record" .= script.id
        ]
      Script.recordApplied table script durationS conn

withLock
  :: (HasField "table" cfg DB.Table, HasField "connString" cfg DB.ConnectionString)
  => Log
  -> cfg
  -> (DB.Connection -> IO b)
  -> IO b
withLock log cfg m = do
  conn <- DB.connect cfg.connString cfg.table
  Log.debug log
    [ "action" .= ("acquire-lock" :: String)
    , "table" .= cfg.table
    ]
  DB.withLock cfg.table conn $ do
    Log.debug log
      [ "action" .= ("try-to-acquire-lock" :: String)
      , "table" .= cfg.table
      , "result" .= ("acquired" :: String)
      ]
    m conn

withTryLock
  :: (HasField "table" cfg DB.Table, HasField "connString" cfg DB.ConnectionString)
  => Log
  -> cfg
  -> (DB.Connection -> IO b) -> IO b
withTryLock log cfg m = do
  conn <- DB.connect cfg.connString cfg.table
  Log.debug log
    [ "action" .= ("try-to-acquire-lock" :: String)
    , "table" .= cfg.table
    ]
  DB.withTryLock cfg.table conn $ \locked -> do
    unless locked $ do
      Log.error log
        [ "action" .= ("try-to-acquire-lock" :: String)
        , "table" .= cfg.table
        , "result" .= ("couldn't lock the database, migration in progress?" :: String)
        ]
      exitFailure
    Log.debug log
      [ "action" .= ("try-to-acquire-lock" :: String)
      , "table" .= cfg.table
      , "result" .= ("acquired" :: String)
      ]
    m conn

loadAll :: DB.Table -> DB.Connection -> FilePath -> IO Migration.Merge.Result
loadAll table conn dir = do
  migrations <- Migration.loadAll table conn
  scripts <- Script.listDirectory dir
  pure (Migration.merge migrations scripts)
