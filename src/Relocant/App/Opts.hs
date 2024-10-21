{-# LANGUAGE ApplicativeDo #-}
module Relocant.App.Opts where

import Options.Applicative

import Relocant.DB (ConnectionString, Table, defaultTable)


data Cmd
  = Unapplied ConnectionString Table FilePath
  | Applied ConnectionString Table
  | Verify ConnectionString Table FilePath Bool
  | Apply ConnectionString Table FilePath
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
   -- command "mark-applied (specific script)" ?
   -- command "mark-unapplied (specific migration)" ?
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
