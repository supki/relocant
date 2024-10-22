{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module Relocant.App.Opts.Internal
  ( InternalCmd(..)
  , parser
  , DumpSchema(..)
  , MarkApplied(..)
  , Delete(..)
  , DeleteAll(..)
  ) where

import Options.Applicative
import Prelude hiding (id)

import Relocant.App.Opts.Option qualified as O
import Relocant.DB (ConnectionString, Table)
import Relocant.Migration qualified as Migration


data InternalCmd
  = DumpSchema DumpSchema
  | MarkApplied MarkApplied
  | Delete Delete
  | DeleteAll DeleteAll
    deriving (Show, Eq)

parser :: Parser InternalCmd
parser =
  hsubparser
    ( command "dump-schema"
      (info dumpSchemaP (progDesc "dump db schema"))
   <> command "mark-applied"
      (info markAppliedP (progDesc "mark a migration applied without running its script"))
   <> command "delete"
      (info deleteP (progDesc "delete an applied migration from the database"))
   <> command "delete-all"
      (info deleteAllP (progDesc "delete all applied migrations from the database"))
    )

data DumpSchema = MkDumpSchema
  { connString :: ConnectionString
  } deriving (Show, Eq)

data MarkApplied = MkMarkApplied
  { connString :: ConnectionString
  , table      :: Table
  , script     :: FilePath
  } deriving (Show, Eq)

data Delete = MkDelete
  { connString :: ConnectionString
  , table      :: Table
  , id         :: Migration.ID
  } deriving (Show, Eq)

data DeleteAll = MkDeleteAll
  { connString :: ConnectionString
  , table      :: Table
  } deriving (Show, Eq)

dumpSchemaP  :: Parser InternalCmd
dumpSchemaP = do
  connString <- O.connectionString
  pure (DumpSchema MkDumpSchema {..})

markAppliedP :: Parser InternalCmd
markAppliedP = do
  connString <- O.connectionString
  table <- O.table
  script <- O.file
  pure (MarkApplied MkMarkApplied {..})

deleteP :: Parser InternalCmd
deleteP = do
  connString <- O.connectionString
  table <- O.table
  id <- O.id
  pure (Delete MkDelete {..})

deleteAllP :: Parser InternalCmd
deleteAllP = do
  connString <- O.connectionString
  table <- O.table
  pure (DeleteAll MkDeleteAll {..})
