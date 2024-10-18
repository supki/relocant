{-# LANGUAGE ApplicativeDo #-}
module Relocant.App.Opts where

import Data.ByteString (ByteString)
import Options.Applicative


data Cmd
  = ListScripts FilePath
  | ListMigrations ByteString
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
    )

listScriptsP :: Parser Cmd
listScriptsP = do
  path <-
    strOption (short 'd' <> long "directory" <> help "Directory containing .sql scripts")
  pure (ListScripts path)

listMigrationsP :: Parser Cmd
listMigrationsP = do
  connectionString <-
    strOption (short 'c' <> long "connection-string" <> help "PostgreSQL connection string")
  pure (ListMigrations connectionString)
