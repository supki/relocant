{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
module Relocant.App.Env
  ( Env(..)
  , Relocant.App.Env.parse
  , Meta.name
  , Meta.version
  ) where

import Env
import Meta_relocant qualified as Meta

import Relocant.DB (defaultTable)
import Relocant.DB.Table (Table)


data Env = Env
  { scripts :: Maybe FilePath
  , table   :: Table
  } deriving (Show, Eq)

parse :: IO Env
parse =
  Env.parse (header usageHeader) parser

parser :: Parser Error Env
parser =
  prefixed "RELOCANT_" $ do
    scripts <-
      optional
        (var str "SCRIPTS_DIR"
          (help "Directory containing .sql scripts"))
    table <-
      var str "MIGRATIONS_TABLE_NAME"
        ( def defaultTable
       <> help "Name of the table containing applied migrations"
        )
    pure Env {..}

usageHeader :: String
usageHeader =
  unwords [Meta.name, Meta.version]
