{-# LANGUAGE ApplicativeDo #-}
module Relocant.App.Opts where

import Options.Applicative
import Prelude hiding (id)

import Relocant.DB (ConnectionString, Table, defaultTable)
import Relocant.Migration qualified as Migration


data Cmd
  = Unapplied ConnectionString Table FilePath
  | Applied ConnectionString Table
  | Verify ConnectionString Table FilePath Bool
  | Apply ConnectionString Table FilePath
  | Version
  | MarkApplied ConnectionString Table FilePath
  | Delete ConnectionString Table Migration.ID
  | DeleteAll ConnectionString Table
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
      (info verifyP (progDesc "verify that there are no unapplied migrations"))
   <> command "apply"
      (info applyP (progDesc "apply the unapplied migrations"))
   <> command "version"
      (info versionP (progDesc "see library's version"))
   <> command "internal"
      (info internalP (progDesc "internal subcomamnds"))
    )
   -- command "dump-schema (as initial migration)"
   -- command "internal-apply (specific script)" ?
   -- environment variables ?
   -- --format (text / json)
   -- --with-content
   -- actual logging

unappliedP :: Parser Cmd
unappliedP = do
  connectionString <- connectionStringO
  table <- tableO
  dir <- directoryO
  pure (Unapplied connectionString table dir)

appliedP :: Parser Cmd
appliedP = do
  connectionString <- connectionStringO
  table <- tableO
  pure (Applied connectionString table)

verifyP :: Parser Cmd
verifyP = do
  connectionString <- connectionStringO
  table <- tableO
  dir <- directoryO
  quiet <- switch (short 'q' <> long "quiet" <> help "do not output the problems")
  pure (Verify connectionString table dir quiet)

applyP :: Parser Cmd
applyP = do
  connectionString <- connectionStringO
  table <- tableO
  dir <- directoryO
  pure (Apply connectionString table dir)

versionP :: Parser Cmd
versionP =
  pure Version

internalP :: Parser Cmd
internalP =
  hsubparser
    ( command "mark-applied"
      (info internalMarkAppliedP (progDesc "mark a migration applied without running its script"))
   <> command "delete"
      (info internalDeleteP (progDesc "delete an applied migration from the database"))
   <> command "delete-all"
      (info internalDeleteAllP (progDesc "delete all applied migrations from the database"))
    )

internalMarkAppliedP :: Parser Cmd
internalMarkAppliedP = do
  connectionString <- connectionStringO
  table <- tableO
  file <- fileO
  pure (MarkApplied connectionString table file)

internalDeleteP :: Parser Cmd
internalDeleteP = do
  connectionString <- connectionStringO
  table <- tableO
  id <- idO
  pure (Delete connectionString table id)

internalDeleteAllP :: Parser Cmd
internalDeleteAllP = do
  connectionString <- connectionStringO
  table <- tableO
  pure (DeleteAll connectionString table)

connectionStringO :: Parser ConnectionString
connectionStringO =
  strOption
    ( short 'c'
   <> long "connection-string"
   <> metavar "CONNECTION_STRING"
   <> help "PostgreSQL connection string"
    )

tableO :: Parser Table
tableO =
  strOption
    ( short 't'
   <> long "migration-table-name"
   <> metavar "IDENTIFIER"
   <> value defaultTable
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

fileO :: Parser String
fileO =
  strOption
    ( short 'f'
   <> long "file"
   <> metavar "PATH"
   <> help "Path to an .sql script"
    )

idO :: Parser Migration.ID
idO =
  strOption
    ( long "id"
   <> metavar "ID"
   <> help "Migration ID"
    )
