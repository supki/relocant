module Relocant.App.Opts.Option
  ( connectionString
  , table
  , scripts
  , script
  , id
  ) where

import Options.Applicative
import Prelude hiding (id)

import Relocant.App.Env (Env(..))
import Relocant.DB (ConnectionString, Table)
import Relocant.Migration qualified as Migration


connectionString :: Parser ConnectionString
connectionString =
  strOption
    ( short 'c'
   <> long "connection-string"
   <> metavar "CONNECTION_STRING"
   <> value ""
   <> help "PostgreSQL connection string"
    )

table :: Env -> Parser Table
table env =
  strOption
    ( short 't'
   <> long "migration-table-name"
   <> metavar "IDENTIFIER"
   <> value env.table
   <> showDefault
   <> help "Name of the table containing applied migrations"
    )

scripts :: Env -> Parser String
scripts env =
  strOption
    ( short 'd'
   <> long "directory"
   <> metavar "PATH"
   <> foldMap value env.scripts
   <> help "Directory containing .sql scripts"
    )

script :: Parser String
script =
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
