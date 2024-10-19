{-# LANGUAGE ApplicativeDo #-}
module Relocant.App.Opts where

import Options.Applicative

import Relocant.DB (ConnectionString)


data Cmd
  = Unapplied ConnectionString FilePath
  | Applied ConnectionString
  | Verify ConnectionString FilePath Bool
  | Apply ConnectionString FilePath
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
   -- --table-name (e.g., 'public.migration')
   -- environment variables ?
   -- --format (text / json)
   -- actual logging

unappliedP :: Parser Cmd
unappliedP = do
  connectionString <- connectionStringO
  dir <- directoryO
  pure (Unapplied connectionString dir)

appliedP :: Parser Cmd
appliedP = do
  connectionString <- connectionStringO
  pure (Applied connectionString)

verifyP :: Parser Cmd
verifyP = do
  connectionString <- connectionStringO
  dir <- directoryO
  quiet <- switch (short 'q' <> long "quiet" <> help "do not output the problems")
  pure (Verify connectionString dir quiet)

applyP :: Parser Cmd
applyP = do
  connectionString <- connectionStringO
  dir <- directoryO
  pure (Apply connectionString dir)

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

directoryO :: Parser String
directoryO =
  strOption
    ( short 'd'
   <> long "directory"
   <> metavar "PATH"
   <> help "Directory containing .sql scripts"
    )
