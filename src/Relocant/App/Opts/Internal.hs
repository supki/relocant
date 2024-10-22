{-# LANGUAGE ApplicativeDo #-}
module Relocant.App.Opts.Internal
  ( InternalCmd(..)
  , parser
  ) where

import Options.Applicative
import Prelude hiding (id)

import Relocant.App.Opts.Option qualified as O
import Relocant.DB (ConnectionString, Table)
import Relocant.Migration qualified as Migration


data InternalCmd
  = DumpSchema ConnectionString
  | MarkApplied ConnectionString Table FilePath
  | Delete ConnectionString Table Migration.ID
  | DeleteAll ConnectionString Table
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

dumpSchemaP  :: Parser InternalCmd
dumpSchemaP = do
  connectionString <- O.connectionString
  pure (DumpSchema connectionString)

markAppliedP :: Parser InternalCmd
markAppliedP = do
  connectionString <- O.connectionString
  table <- O.table
  file <- O.file
  pure (MarkApplied connectionString table file)

deleteP :: Parser InternalCmd
deleteP = do
  connectionString <- O.connectionString
  table <- O.table
  id <- O.id
  pure (Delete connectionString table id)

deleteAllP :: Parser InternalCmd
deleteAllP = do
  connectionString <- O.connectionString
  table <- O.table
  pure (DeleteAll connectionString table)
