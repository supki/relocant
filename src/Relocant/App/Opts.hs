{-# LANGUAGE ApplicativeDo #-}
module Relocant.App.Opts
  ( Cmd(..)
  , InternalCmd(..)
  , parse
  ) where

import Options.Applicative
import Prelude hiding (id)

import Relocant.DB (ConnectionString, Table)
import Relocant.App.Opts.Option qualified as O
import Relocant.App.Opts.Internal (InternalCmd(..))
import Relocant.App.Opts.Internal qualified as Internal (parser)


data Cmd
  = Unapplied ConnectionString Table FilePath
  | Applied ConnectionString Table
  | Verify ConnectionString Table FilePath Bool
  | Apply ConnectionString Table FilePath
  | Version
  | Internal InternalCmd
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
   -- environment variables ?
   -- --format (text / json)
   -- --with-content
   -- actual logging

unappliedP :: Parser Cmd
unappliedP = do
  connectionString <- O.connectionString
  table <- O.table
  dir <- O.directory
  pure (Unapplied connectionString table dir)

appliedP :: Parser Cmd
appliedP = do
  connectionString <- O.connectionString
  table <- O.table
  pure (Applied connectionString table)

verifyP :: Parser Cmd
verifyP = do
  connectionString <- O.connectionString
  table <- O.table
  dir <- O.directory
  quiet <- switch (short 'q' <> long "quiet" <> help "do not output the problems")
  pure (Verify connectionString table dir quiet)

applyP :: Parser Cmd
applyP = do
  connectionString <- O.connectionString
  table <- O.table
  dir <- O.directory
  pure (Apply connectionString table dir)

versionP :: Parser Cmd
versionP =
  pure Version

internalP :: Parser Cmd
internalP =
  fmap Internal Internal.parser
