module Relocant.App.Opts.Option
  ( connectionString
  , table
  , directory
  , file
  , id
  ) where

import Options.Applicative
import Prelude hiding (id)

import Relocant.DB (ConnectionString, Table, defaultTable)
import Relocant.Migration qualified as Migration


connectionString :: Parser ConnectionString
connectionString =
  strOption
    ( short 'c'
   <> long "connection-string"
   <> metavar "CONNECTION_STRING"
   <> help "PostgreSQL connection string"
    )

table :: Parser Table
table =
  strOption
    ( short 't'
   <> long "migration-table-name"
   <> metavar "IDENTIFIER"
   <> value defaultTable
   <> showDefault
   <> help "Table containing recorded migrations"
    )

directory :: Parser String
directory =
  strOption
    ( short 'd'
   <> long "directory"
   <> metavar "PATH"
   <> help "Directory containing .sql scripts"
    )

file :: Parser String
file =
  strOption
    ( short 'f'
   <> long "file"
   <> metavar "PATH"
   <> help "Path to an .sql script"
    )

id :: Parser Migration.ID
id =
  strOption
    ( long "id"
   <> metavar "ID"
   <> help "Migration ID"
    )
