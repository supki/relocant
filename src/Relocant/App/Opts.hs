{-# LANGUAGE ApplicativeDo #-}
module Relocant.App.Opts where

import Data.ByteString (ByteString)
import Options.Applicative


data Cmd
  = ListScripts FilePath
  | ListMigrations ByteString
  | DryRun ByteString FilePath
    deriving (Show, Eq)

parse :: IO Cmd
parse =
  execParser
    (info
      (parser <**> helper)
      (fullDesc <> progDesc "Migrate PostgreSQL database" <> header "relocant - migrating utility"))

parser :: Parser Cmd
parser =
  hsubparser
    ( command "list-scripts"
      (info listScriptsP (progDesc "list migration scripts"))
   <> command "list-migrations"
      (info listMigrationsP (progDesc "list applied migrations"))
   <> command "dry-run"
      (info dryRunP (progDesc "see which migrations are going to be applied without applying them"))
    )

listScriptsP :: Parser Cmd
listScriptsP = do
  dir <-
    strOption (short 'd' <> long "directory" <> help "Directory containing .sql scripts")
  pure (ListScripts dir)

listMigrationsP :: Parser Cmd
listMigrationsP = do
  connectionString <-
    strOption (short 'c' <> long "connection-string" <> help "PostgreSQL connection string")
  pure (ListMigrations connectionString)

dryRunP :: Parser Cmd
dryRunP = do
  connectionString <-
    strOption (short 'c' <> long "connection-string" <> help "PostgreSQL connection string")
  dir <-
    strOption (short 'd' <> long "directory" <> help "Directory containing .sql scripts")
  pure (DryRun connectionString dir)
