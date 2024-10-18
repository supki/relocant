{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Relocant.Script
  ( Script(..)
  , ID
  , Name
  , Content
  , listDirectory
  , readFile
  , recordApplied
  ) where

import "crypton" Crypto.Hash (Digest, SHA1, hash)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Char qualified as Char
import Data.List qualified as List
import Data.String (fromString)
import Database.PostgreSQL.Simple qualified as DB
import Database.PostgreSQL.Simple.ToField qualified as DB (ToField)
import Database.PostgreSQL.Simple.SqlQQ qualified as DB (sql)
import GHC.Records (HasField(getField))
import Prelude hiding (id, readFile)
import System.Directory qualified as D
import System.FilePath ((</>), isExtensionOf, takeBaseName)

import Relocant.Migration.ID qualified as Migration (ID)


data Script = Script
  { id      :: Migration.ID
  , name    :: Name
  , content :: Content
  } deriving (Show, Eq)

instance HasField "bytes" Script ByteString where
  getField m =
    m.content.bytes

instance HasField "sha1" Script (Digest SHA1) where
  getField m =
    m.content.sha1

newtype ID = ID String
    deriving (Show, Eq, Ord, DB.ToField)

newtype Name = Name String
    deriving (Show, Eq, DB.ToField)

data Content = Content
  { bytes :: ByteString
  , sha1  :: Digest SHA1
  } deriving (Show, Eq)

listDirectory :: FilePath -> IO [Script]
listDirectory dir = do
  paths <- D.listDirectory dir
  scripts <- traverse (\path -> readFile (dir </> path)) (filter (isExtensionOf ".sql") paths)
  pure (List.sortOn (\script -> script.id) scripts)

readFile :: FilePath -> IO Script
readFile path = do
  let
    (id, name) =
      parseFilePath path
  content <- readContent path
  pure Script
    { id
    , name
    , content
    }

readContent :: FilePath -> IO Content
readContent path = do
  bytes <- ByteString.readFile path
  pure Content
    { bytes
    , sha1 = hash bytes
    }

parseFilePath :: FilePath -> (Migration.ID, Name)
parseFilePath path = do
  let
    basename =
      takeBaseName path
    (id, _rest) =
      span Char.isAlphaNum basename
  (fromString id, Name basename)

recordApplied :: Script -> DB.Connection -> IO ()
recordApplied m conn = do
  1 <- DB.execute conn [DB.sql|
    INSERT INTO migration
              ( id
              , name
              , bytes
              , sha1
              , applied_at
              )
         SELECT ?
              , ?
              , ?
              , ?
              , CURRENT_TIMESTAMP
  |] ( m.id
     , m.name
     , m.bytes
     , DB.Binary (convert @_ @ByteString m.sha1)
     )
  pure ()
