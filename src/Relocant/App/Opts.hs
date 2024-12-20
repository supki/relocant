{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK hide #-}
module Relocant.App.Opts
  ( Cfg(..)
  , Cmd(..)
  , InternalCmd(..)
  , parse
  , ListUnapplied(..)
  , ListApplied(..)
  , ShowApplied(..)
  , Verify(..)
  , Apply(..)
  , Internal.DumpSchema(..)
  , Internal.MarkApplied(..)
  , Internal.Delete(..)
  , Internal.DeleteAll(..)
  ) where

import Data.Aeson qualified as Aeson
import GHC.Generics (Generic, Rep)
import Options.Applicative
import Prelude hiding (id)

import Meta_relocant qualified as Meta
import Relocant.App.Env (Env)
import Relocant.App.Log qualified as Log
import Relocant.App.Opts.Fmt (Fmt)
import Relocant.App.Opts.Option qualified as O
import Relocant.App.Opts.Internal (InternalCmd(..))
import Relocant.App.Opts.Internal qualified as Internal
import Relocant.DB (ConnectionString, Table)
import Relocant.ID (ID)


data Cfg = Cfg
  { minSeverity :: Log.Severity
  } deriving (Show, Eq)

data Cmd
  = ListUnapplied ListUnapplied
  | ListApplied ListApplied
  | ShowApplied ShowApplied
  | Verify Verify
  | Apply Apply
  | Version String
  | Internal InternalCmd
    deriving (Show, Eq)

data ListUnapplied = MkListUnapplied
  { connString :: ConnectionString
  , table      :: Table
  , scripts    :: FilePath
  , format     :: Fmt
  } deriving (Show, Eq, Generic)

instance Aeson.ToJSON ListUnapplied where
  toJSON = toJSONG

data ListApplied = MkListApplied
  { connString :: ConnectionString
  , table      :: Table
  , format     :: Fmt
  } deriving (Show, Eq, Generic)

instance Aeson.ToJSON ListApplied where
  toJSON = toJSONG

data ShowApplied = MkShowApplied
  { connString :: ConnectionString
  , table      :: Table
  , id         :: ID
  } deriving (Show, Eq, Generic)

instance Aeson.ToJSON ShowApplied where
  toJSON = toJSONG

data Verify = MkVerify
  { connString :: ConnectionString
  , table      :: Table
  , scripts    :: FilePath
  , quiet      :: Bool
  , format     :: Fmt
  } deriving (Show, Eq, Generic)

instance Aeson.ToJSON Verify where
  toJSON = toJSONG

data Apply = MkApply
  { connString :: ConnectionString
  , table      :: Table
  , scripts    :: FilePath
  , format     :: Fmt
  } deriving (Show, Eq, Generic)

instance Aeson.ToJSON Apply where
  toJSON = toJSONG

toJSONG :: (Generic a, Aeson.GToJSON' Aeson.Value Aeson.Zero (Rep a)) => a -> Aeson.Value
toJSONG =
  Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = \case
        "table" -> "migrations-table-name"
        "connString" -> "connection-string"
        label -> label
    }

parse :: Env -> IO (Cfg, Cmd)
parse env =
  customExecParser defaultPrefs {prefShowHelpOnError = True}
    (info
      (parser env <**> helper)
      (fullDesc <> progDesc "Migrate PostgreSQL database" <> header "relocant - migrating utility"))

parser :: Env -> Parser (Cfg, Cmd)
parser env = do
  cfg <- cfgP
  cmd <- hsubparser
    ( command "list-unapplied"
      (info (listUnappliedP env) (progDesc "list unapplied migrations"))
   <> command "list-applied"
      (info (listAppliedP env) (progDesc "list applied migrations"))
   <> command "show-applied"
      (info (showAppliedP env) (progDesc "show the contents of an applied migration"))
   <> command "verify"
      (info (verifyP env) (progDesc "verify that there are no unapplied migrations"))
   <> command "apply"
      (info (applyP env) (progDesc "apply the unapplied migrations"))
   <> command "version"
      (info versionP (progDesc "see library's version"))
   <> command "internal"
      (info (internalP env) (progDesc "internal subcomamnds"))
    )
  pure (cfg, cmd)

cfgP :: Parser Cfg
cfgP = do
  minSeverity <- O.minSeverity
  pure Cfg {..}

listUnappliedP :: Env -> Parser Cmd
listUnappliedP env = do
  connString <- O.connectionString
  table <- O.table env
  scripts <- O.scripts env
  format <- O.fmt
  pure (ListUnapplied MkListUnapplied {..})

listAppliedP :: Env -> Parser Cmd
listAppliedP env = do
  connString <- O.connectionString
  table <- O.table env
  format <- O.fmt
  pure (ListApplied MkListApplied {..})

showAppliedP :: Env -> Parser Cmd
showAppliedP env = do
  connString <- O.connectionString
  table <- O.table env
  id <- O.id
  pure (ShowApplied MkShowApplied {..})

verifyP :: Env -> Parser Cmd
verifyP env = do
  connString <- O.connectionString
  table <- O.table env
  scripts <- O.scripts env
  quiet <- switch (short 'q' <> long "quiet" <> help "do not output the problems")
  format <- O.fmt
  pure (Verify MkVerify {..})

applyP :: Env -> Parser Cmd
applyP env = do
  connString <- O.connectionString
  table <- O.table env
  scripts <- O.scripts env
  format <- O.fmt
  pure (Apply MkApply {..})

versionP :: Parser Cmd
versionP =
  pure (Version Meta.version)

internalP :: Env -> Parser Cmd
internalP =
  fmap Internal . Internal.parser
