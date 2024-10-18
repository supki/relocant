{-# LANGUAGE ApplicativeDo #-}
module Relocant.App.Opts where

import Data.ByteString (ByteString)
import Options.Applicative


data Cmd
  = Unapplied ByteString FilePath
  | Applied ByteString
  | Verify ByteString FilePath
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
   <> command "version"
      (info versionP (progDesc "see library's version"))
    )
   -- command "apply (unapplied)"
   -- command "dump-schema (as initial migration)"
   -- --with-content
   -- command "mark-applied (specific script)" ?
   -- command "mark-unapplied (specific migration)" ?
   -- --table-name (e.g., 'public.migration')
   -- environment variables ?
   -- --format

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
  pure (Verify connectionString dir)

versionP :: Parser Cmd
versionP =
  pure Version

connectionStringO :: Parser ByteString
connectionStringO =
  strOption (short 'c' <> long "connection-string" <> help "PostgreSQL connection string")

directoryO :: Parser String
directoryO =
  strOption (short 'd' <> long "directory" <> help "Directory containing .sql scripts")
