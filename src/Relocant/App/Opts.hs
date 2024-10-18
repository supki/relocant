{-# LANGUAGE ApplicativeDo #-}
module Relocant.App.Opts where

import Options.Applicative


data Cmd
  = ListScripts FilePath
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
    )

listScriptsP :: Parser Cmd
listScriptsP = do
  path <- strOption (short 'd' <> long "directory" <> help "Directory containing .sql scripts")
  pure (ListScripts path)
