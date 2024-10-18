module Relocant.App
  ( run
  ) where

import Data.Foldable (traverse_)
import Database.PostgreSQL.Simple qualified as DB

import Relocant.App.Opts qualified as Opts
import Relocant.Migration qualified as Migration
import Relocant.Script qualified as Script


run :: IO ()
run = do
  cmd <- Opts.parse
  case cmd of
    Opts.ListScripts dir -> do
      scripts <- Script.listDirectory dir
      traverse_ print scripts
    Opts.ListMigrations connectionString -> do
      conn <- DB.connectPostgreSQL connectionString
      migrations <- Migration.loadAll conn
      traverse_ print migrations
