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
import Relocant.App.Env (Env)
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

parse :: Env -> IO Cmd
parse env =
  customExecParser defaultPrefs {prefShowHelpOnError = True}
    (info
      (parser env <**> helper)
      (fullDesc <> progDesc "Migrate PostgreSQL database" <> header "relocant - migrating utility"))

parser :: Env -> Parser Cmd
parser env =
  hsubparser
    ( command "unapplied"
      (info (unappliedP env) (progDesc "list unapplied migrations"))
   <> command "applied"
      (info (appliedP env) (progDesc "list applied migrations"))
   <> command "verify"
      (info (verifyP env) (progDesc "verify that there are no unapplied migrations"))
   <> command "apply"
      (info (applyP env) (progDesc "apply the unapplied migrations"))
   <> command "version"
      (info versionP (progDesc "see library's version"))
   <> command "internal"
      (info (internalP env) (progDesc "internal subcomamnds"))
    )

unappliedP :: Env -> Parser Cmd
unappliedP env = do
  connString <- O.connectionString
  table <- O.table env
  scripts <- O.scripts env
  pure (Unapplied MkUnapplied {..})

appliedP :: Env -> Parser Cmd
appliedP env = do
  connString <- O.connectionString
  table <- O.table env
  pure (Applied MkApplied {..})

verifyP :: Env -> Parser Cmd
verifyP env = do
  connString <- O.connectionString
  table <- O.table env
  scripts <- O.scripts env
  quiet <- switch (short 'q' <> long "quiet" <> help "do not output the problems")
  pure (Verify MkVerify {..})

applyP :: Env -> Parser Cmd
applyP env = do
  connString <- O.connectionString
  table <- O.table env
  scripts <- O.scripts env
  pure (Apply MkApply {..})

versionP :: Parser Cmd
versionP =
  pure (Version Meta.version)

internalP :: Env -> Parser Cmd
internalP =
  fmap Internal . Internal.parser
