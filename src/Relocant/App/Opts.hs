{-# LANGUAGE ApplicativeDo #-}
module Relocant.App.Opts where

import Options.Applicative

import Relocant.DB (ConnectionString, TableName)


data Cmd
  = Unapplied ConnectionString TableName FilePath
  | Applied ConnectionString TableName
  | Verify ConnectionString TableName FilePath Bool
  | Apply ConnectionString TableName FilePath
  | Version
    deriving (Show, Eq)

parse :: IO Cmd
parse =
  customExecParser defaultPrefs {prefShowHelpOnError = True}
    (info
      (parser <**> helper)
      (fullDesc <> progDesc "Migrate PostgreSQL database" <> header "relocant - migrating utility"))

parser :: Parser Cmd
parser =
  hsubparser
    ( command "unapplied"
      (info unappliedP (progDesc "list unapplied migrations"))
   <> command "applied"
      (info appliedP (progDesc "list applied migrations"))
   <> command "verify"
      (info verifyP (progDesc "verify that all migrations that are supposed to be applied are and those that aren't aren't"))
   <> command "apply"
      (info applyP (progDesc "apply the unapplied migrations"))
   <> command "version"
      (info versionP (progDesc "see library's version"))
    )
   -- command "dump-schema (as initial migration)"
   -- --with-content
   -- command "mark-applied (specific script)" ?
   -- command "mark-unapplied (specific migration)" ?
   -- environment variables ?
   -- --format (text / json)
   -- actual logging
   -- track execution time

unappliedP :: Parser Cmd
unappliedP = do
  connectionString <- connectionStringO
  tableName <- tableNameO
  dir <- directoryO
  pure (Unapplied connectionString tableName dir)

appliedP :: Parser Cmd
appliedP = do
  connectionString <- connectionStringO
  tableName <- tableNameO
  pure (Applied connectionString tableName)

verifyP :: Parser Cmd
verifyP = do
  connectionString <- connectionStringO
  tableName <- tableNameO
  dir <- directoryO
  quiet <- switch (short 'q' <> long "quiet" <> help "do not output the problems")
  pure (Verify connectionString tableName dir quiet)

applyP :: Parser Cmd
applyP = do
  connectionString <- connectionStringO
  tableName <- tableNameO
  dir <- directoryO
  pure (Apply connectionString tableName dir)

versionP :: Parser Cmd
versionP =
  pure Version

connectionStringO :: Parser ConnectionString
connectionStringO =
  strOption
    ( short 'c'
   <> long "connection-string"
   <> metavar "CONNECTION_STRING"
   <> help "PostgreSQL connection string"
    )

tableNameO :: Parser TableName
tableNameO =
  strOption
    ( short 't'
   <> long "migration-table-name"
   <> metavar "IDENTIFIER"
   <> value "public.relocant_migration"
   <> showDefault
   <> help "Table containing recorded migrations"
    )

directoryO :: Parser String
directoryO =
  strOption
    ( short 'd'
   <> long "directory"
   <> metavar "PATH"
   <> help "Directory containing .sql scripts"
    )
