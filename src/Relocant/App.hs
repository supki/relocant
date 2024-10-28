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

import Relocant qualified
import Relocant.App.Env qualified as Env
import Relocant.App.Log (Log)
import Relocant.App.Log qualified as Log
import Relocant.App.Opts qualified as Opts
import Relocant.App.Opts.Fmt (Fmt)
import Relocant.App.Opts.Fmt qualified as Fmt
import Relocant.App.ToText (ToText(..))
import Relocant.DB qualified as DB (ConnectionString, Table, connect, dumpSchema, withLock, withTryLock)
import Relocant.Migration.Applied qualified as Applied (selectByID, deleteAll, deleteByID)
import Relocant.Script qualified as Script (readFile, markApplied)


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
    migrations <- Relocant.mergeAll opts.table conn opts.scripts
    traverse_ (println opts.format) migrations.unapplied

runListApplied :: Log -> Opts.ListApplied -> IO ()
runListApplied log opts = do
  withTryLock log opts $ \conn -> do
    migrations <- Relocant.getApplied opts.table conn
    traverse_ (println opts.format) migrations

runShowApplied :: Log -> Opts.ShowApplied -> IO ()
runShowApplied log opts = do
  withTryLock log opts $ \conn -> do
    applied <- Applied.selectByID opts.id opts.table conn
    maybe exitFailure (\a -> ByteString.putStr a.bytes) applied

runVerify :: Log -> Opts.Verify -> IO ()
runVerify log opts = do
  withTryLock log opts $ \conn -> do
    verify log opts.table conn opts.scripts (opts.format <$ guard (not opts.quiet))

runApply :: Log -> Opts.Apply -> IO ()
runApply log opts = do
  withLock log opts $ \conn -> do
    apply log opts.table conn opts.scripts opts.format
    verify log opts.table conn opts.scripts Nothing

runDumpSchema :: Opts.DumpSchema -> IO ()
runDumpSchema _opts =
  DB.dumpSchema

runMarkApplied :: Log -> Opts.MarkApplied -> IO ()
runMarkApplied log opts = do
  withTryLock log opts $ \conn -> do
    script <- Script.readFile opts.script
    DB.withTransaction conn $ do
      Log.debug log
        [ "action" .= ("mark-applied" :: String)
        , "delete" .= script.id
        ]
      _deleted <- Applied.deleteByID script.id opts.table conn
      Log.debug log
        [ "action" .= ("mark-applied" :: String)
        , "record" .= script.id
        ]
      markApplied <- Script.markApplied script
      Relocant.record markApplied opts.table conn

runDelete :: Log -> Opts.Delete -> IO ()
runDelete log opts = do
  withTryLock log opts $ \conn -> do
    deleted <- Applied.deleteByID opts.id opts.table conn
    unless deleted $ do
      Log.notice log
        [ "action" .= ("delete" :: String)
        , "result" .= ("couldn't delete the recorded migration, typo in the ID?" :: String)
        ]
      exitFailure

runDeleteAll :: Log -> Opts.DeleteAll -> IO ()
runDeleteAll log opts = do
  withTryLock log opts $ \conn -> do
    Applied.deleteAll opts.table conn

verify :: Log -> DB.Table -> DB.Connection -> FilePath -> Maybe Fmt -> IO ()
verify log table conn dir formatQ = do
  migrations <- Relocant.mergeAll table conn dir
  unless (Relocant.converged migrations) $ do
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

apply :: Log -> DB.Table -> DB.Connection -> FilePath -> Fmt -> IO ()
apply log table conn scripts format = do
  merged <- Relocant.mergeAll table conn scripts
  unless (Relocant.canApply merged) $ do
    Log.error log
      [ "action" .= ("apply" :: String)
      , "result" .= ("not ready to apply the scripts, run `verify' first" :: String)
      ]
    exitFailure
  Log.debug log
    [ "action" .= ("apply" :: String)
    , "migrations" .= map (.id) merged.unapplied
    ]
  for_ merged.unapplied $ \script -> do
    println format script
    DB.withTransaction conn $ do
      Log.debug log
        [ "action" .= ("apply" :: String)
        , "apply" .= script.id
        ]
      applied <- Relocant.apply script conn
      Log.debug log
        [ "action" .= ("apply" :: String)
        , "record" .= script.id
        , "duration" .= applied.durationS
        ]
      Relocant.record applied table conn
      println format applied

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

println :: (Aeson.ToJSON a, ToText a) => Fmt -> a -> IO ()
println = \case
  Fmt.Json ->
    ByteString.Lazy.putStrLn . Aeson.encode
  Fmt.Text ->
    Text.putStrLn . toText
