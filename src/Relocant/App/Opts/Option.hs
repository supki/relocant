{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_HADDOCK hide #-}
module Relocant.App.Opts.Option
  ( fmt
  , minSeverity
  , connectionString
  , table
  , scripts
  , script
  , id
  ) where

import Options.Applicative
import Prelude hiding (id)

import Relocant.App.Env (Env(..))
import Relocant.App.Log qualified as Log
import Relocant.App.Opts.Fmt (Fmt)
import Relocant.App.Opts.Fmt qualified as Fmt
import Relocant.DB (ConnectionString, Table)
import Relocant.ID (ID)


fmt :: Parser Fmt
fmt =
  option Fmt.reader
    ( long "format"
   <> value Fmt.Text
    )

minSeverity :: Parser Log.Severity
minSeverity =
  option severityR
    ( long "log-min-severity"
   <> value Log.Info
   <> showDefaultWith (\_ -> "info")
    )
 where
  severityR =
    eitherReader $ \case
      "debug" ->
        pure Log.Debug
      "info" ->
        pure Log.Info
      "notice" ->
        pure Log.Notice
      "warning" ->
        pure Log.Warning
      "error" ->
        pure Log.Error
      _ ->
        Left "unknown severity; possible values: debug, info, notice, warning, error"

connectionString :: Parser ConnectionString
connectionString =
  strOption
    ( short 'c'
   <> long "connection-string"
   <> metavar "CONNECTION_STRING"
   <> value ""
   <> help "PostgreSQL connection string"
    )

table :: Env -> Parser Table
table env =
  strOption
    ( short 't'
   <> long "migration-table-name"
   <> metavar "IDENTIFIER"
   <> value env.table
   <> showDefault
   <> help "Name of the table containing applied migrations"
    )

scripts :: Env -> Parser String
scripts env =
  strOption
    ( short 's'
   <> long "scripts"
   <> metavar "PATH"
   <> foldMap value env.scripts
   <> help "Directory containing .sql scripts"
    )

script :: Parser String
script =
  strOption
    ( short 'f'
   <> long "file"
   <> metavar "PATH"
   <> help "Path to an .sql script"
    )

id :: Parser ID
id =
  strOption
    ( long "id"
   <> metavar "ID"
   <> help "Migration ID"
    )
