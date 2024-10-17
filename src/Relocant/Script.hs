{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Relocant.Script
  ( Script(..)
  , ID
  , Name
  , Content
  , readFile
  , recordApplied
  ) where

import "crypton" Crypto.Hash (Digest, SHA1, hash)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Char qualified as Char
import Database.PostgreSQL.Simple qualified as DB
import Database.PostgreSQL.Simple.ToField qualified as DB (ToField)
import Database.PostgreSQL.Simple.SqlQQ qualified as DB (sql)
import GHC.Records (HasField(getField))
import Prelude hiding (id, readFile)
import System.FilePath (takeBaseName)


data Script = Script
  { id      :: ID
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
    deriving (Show, Eq, DB.ToField)

newtype Name = Name String
    deriving (Show, Eq, DB.ToField)

data Content = Content
  { bytes :: ByteString
  , sha1  :: Digest SHA1
  } deriving (Show, Eq)

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

parseFilePath :: FilePath -> (ID, Name)
parseFilePath path = do
  let
    basename =
      takeBaseName path
    (id, _rest) =
      span Char.isAlphaNum basename
  (ID id, Name basename)

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
