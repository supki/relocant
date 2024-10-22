{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module Relocant.App.Opts
  ( Cmd(..)
  , InternalCmd(..)
  , parse
  , Unapplied(..)
  , Applied(..)
  , Verify(..)
  , Apply(..)
  , Internal.DumpSchema(..)
  , Internal.MarkApplied(..)
  , Internal.Delete(..)
  , Internal.DeleteAll(..)
  ) where

import Options.Applicative
import Prelude hiding (id)

import Meta_relocant qualified as Meta
import Relocant.DB (ConnectionString, Table)
import Relocant.App.Opts.Option qualified as O
import Relocant.App.Opts.Internal (InternalCmd(..))
import Relocant.App.Opts.Internal qualified as Internal


data Cmd
  = Unapplied Unapplied
  | Applied Applied
  | Verify Verify
  | Apply Apply
  | Version String
  | Internal InternalCmd
    deriving (Show, Eq)

data Unapplied = MkUnapplied
  { connString :: ConnectionString
  , table      :: Table
  , scripts    :: FilePath
  } deriving (Show, Eq)

data Applied = MkApplied
  { connString :: ConnectionString
  , table      :: Table
  } deriving (Show, Eq)

data Verify = MkVerify
  { connString :: ConnectionString
  , table      :: Table
  , scripts    :: FilePath
  , quiet      :: Bool
  } deriving (Show, Eq)

data Apply = MkApply
  { connString :: ConnectionString
  , table      :: Table
  , scripts    :: FilePath
  } deriving (Show, Eq)

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
  connString <- O.connectionString
  table <- O.table
  scripts <- O.directory
  pure (Unapplied MkUnapplied {..})

appliedP :: Parser Cmd
appliedP = do
  connString <- O.connectionString
  table <- O.table
  pure (Applied MkApplied {..})

verifyP :: Parser Cmd
verifyP = do
  connString <- O.connectionString
  table <- O.table
  scripts <- O.directory
  quiet <- switch (short 'q' <> long "quiet" <> help "do not output the problems")
  pure (Verify MkVerify {..})

applyP :: Parser Cmd
applyP = do
  connString <- O.connectionString
  table <- O.table
  scripts <- O.directory
  pure (Apply MkApply {..})

versionP :: Parser Cmd
versionP =
  pure (Version Meta.version)

internalP :: Parser Cmd
internalP =
  fmap Internal Internal.parser
