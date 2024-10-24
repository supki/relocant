{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Relocant.App
  ( run
  ) where

import Control.Monad (guard, unless)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as ByteString (putStr)
import Data.ByteString.Lazy.Char8 qualified as ByteString.Lazy (putStrLn)
import Data.Foldable (for_, traverse_)
import Data.Text.IO qualified as Text
import Database.PostgreSQL.Simple qualified as DB (Connection, withTransaction)
import GHC.Records (HasField)
import Prelude hiding (id, log)
import System.Exit (exitFailure)

import Relocant.App.Env qualified as Env
import Relocant.App.Log (Log)
import Relocant.App.Log qualified as Log
import Relocant.App.Opts qualified as Opts
import Relocant.App.Opts.Fmt (Fmt)
import Relocant.App.Opts.Fmt qualified as Fmt
import Relocant.App.ToText (ToText(..))
import Relocant.DB qualified as DB
import Relocant.Migration qualified as Migration
import Relocant.Migration.Interval (makeInterval_, zeroInterval)
import Relocant.Migration.Merge qualified as Migration (merge)
import Relocant.Migration.Merge qualified as Migration.Merge
import Relocant.Script qualified as Script


-- --with-content? we probably want to have separate commands for looking into a specific script/migration
--   applied --id ?

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
    runListUnapplied log opts
  Opts.ListApplied opts -> do
    Log.debug log
      [ "action" .= ("list-applied" :: String)
      , "opts" .= opts
      ]
    runListApplied log opts
  Opts.ShowApplied opts -> do
    Log.debug log
      [ "action" .= ("show-applied" :: String)
      , "opts" .= opts
      ]
    runShowApplied log opts
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

runListUnapplied :: Log -> Opts.ListUnapplied -> IO ()
runListUnapplied log opts = do
  withTryLock log opts $ \conn -> do
    migrations <- loadAll opts.table conn opts.scripts
    traverse_ (println opts.format) migrations.unapplied

runListApplied :: Log -> Opts.ListApplied -> IO ()
runListApplied log opts = do
  withTryLock log opts $ \conn -> do
    migrations <- Migration.loadAll opts.table conn
    traverse_ (println opts.format) migrations

runShowApplied :: Log -> Opts.ShowApplied -> IO ()
runShowApplied log opts = do
  withTryLock log opts $ \conn -> do
    migration <- Migration.loadByID opts.id opts.table conn
    maybe exitFailure (\m -> ByteString.putStr m.bytes) migration

runVerify :: Log -> Opts.Verify -> IO ()
runVerify log opts = do
  withTryLock log opts $ \conn -> do
    verify log opts.table conn opts.scripts (opts.format <$ guard (not opts.quiet))

runApply :: Log -> Opts.Apply -> IO ()
runApply log opts = do
  withLock log opts $ \conn -> do
    apply log opts.table conn opts.scripts
    verify log opts.table conn opts.scripts Nothing

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

verify :: Log -> DB.Table -> DB.Connection -> FilePath -> Maybe Fmt -> IO ()
verify log table conn dir formatQ = do
  migrations <- loadAll table conn dir
  unless (Migration.Merge.converged migrations) $ do
    Log.debug log
      [ "action" .= ("apply" :: String)
      , "unrecorded" .= map (.id) migrations.unrecorded
      , "script-missing" .= map (.id) migrations.scriptMissing
      , "content-mismatch" .= map (\cm -> (cm.expected.id, cm.butGot.id)) migrations.contentMismatch
      , "unapplied" .= map (.id) migrations.unapplied
      ]
    for_ formatQ $ \format -> do
      println format migrations
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

println :: (Aeson.ToJSON a, ToText a) => Fmt -> a -> IO ()
println = \case
  Fmt.Json ->
    ByteString.Lazy.putStrLn . Aeson.encode
  Fmt.Text ->
    Text.putStrLn . toText
