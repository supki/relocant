{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK hide #-}
module Relocant.App.Opts.Internal
  ( InternalCmd(..)
  , parser
  , DumpSchema(..)
  , MarkApplied(..)
  , Delete(..)
  , DeleteAll(..)
  ) where

import Data.Aeson qualified as Aeson
import GHC.Generics (Generic, Rep)
import Options.Applicative
import Prelude hiding (id)

import Relocant.App.Env (Env)
import Relocant.App.Opts.Option qualified as O
import Relocant.DB (ConnectionString, Table)
import Relocant.ID (ID)


data InternalCmd
  = DumpSchema DumpSchema
  | MarkApplied MarkApplied
  | Delete Delete
  | DeleteAll DeleteAll
    deriving (Show, Eq)

parser :: Env -> Parser InternalCmd
parser env =
  hsubparser
    ( command "dump-schema"
      (info (dumpSchemaP env) (progDesc "dump db schema"))
   <> command "mark-applied"
      (info (markAppliedP env)
        (progDesc "mark a migration applied without running its script"))
   <> command "delete"
      (info (deleteP env) (progDesc "delete an applied migration from the database"))
   <> command "delete-all"
      (info (deleteAllP env) (progDesc "delete all applied migrations from the database"))
    )

data DumpSchema = MkDumpSchema
  { connString :: ConnectionString
  , table      :: Table
  } deriving (Show, Eq, Generic)

instance Aeson.ToJSON DumpSchema where
  toJSON = toJSONG

data MarkApplied = MkMarkApplied
  { connString :: ConnectionString
  , table      :: Table
  , script     :: FilePath
  } deriving (Show, Eq, Generic)

instance Aeson.ToJSON MarkApplied where
  toJSON = toJSONG

data Delete = MkDelete
  { connString :: ConnectionString
  , table      :: Table
  , id         :: ID
  } deriving (Show, Eq, Generic)

instance Aeson.ToJSON Delete where
  toJSON = toJSONG

data DeleteAll = MkDeleteAll
  { connString :: ConnectionString
  , table      :: Table
  } deriving (Show, Eq, Generic)

instance Aeson.ToJSON DeleteAll where
  toJSON = toJSONG

toJSONG :: (Generic a, Aeson.GToJSON' Aeson.Value Aeson.Zero (Rep a)) => a -> Aeson.Value
toJSONG =
  Aeson.genericToJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = \case
        "table" -> "migrations-table-name"
        "connString" -> "connection-string"
        label -> label
    }

dumpSchemaP :: Env -> Parser InternalCmd
dumpSchemaP env = do
  connString <- O.connectionString
  table <- O.table env
  pure (DumpSchema MkDumpSchema {..})

markAppliedP :: Env -> Parser InternalCmd
markAppliedP env = do
  connString <- O.connectionString
  table <- O.table env
  script <- O.script
  pure (MarkApplied MkMarkApplied {..})

deleteP :: Env -> Parser InternalCmd
deleteP env = do
  connString <- O.connectionString
  table <- O.table env
  id <- O.id
  pure (Delete MkDelete {..})

deleteAllP :: Env -> Parser InternalCmd
deleteAllP env = do
  connString <- O.connectionString
  table <- O.table env
  pure (DeleteAll MkDeleteAll {..})
