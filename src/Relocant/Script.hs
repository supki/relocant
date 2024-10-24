{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Relocant.Script
  ( Script(..)
  , Content(..)
  , readAll
  , readFile
  , parseFilePath
  , readContent
  , apply
  , record
  ) where

import "crypton" Crypto.Hash (Digest, SHA1, hash)
import Data.Aeson qualified as Aeson
import Data.Aeson ((.=))
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Char qualified as Char
import Data.List qualified as List
import Data.String (IsString(..))
import Database.PostgreSQL.Simple qualified as DB
import Database.PostgreSQL.Simple.SqlQQ qualified as DB (sql)
import Database.PostgreSQL.Simple.Types qualified as DB (Query(..))
import GHC.Records (HasField(getField))
import Prelude hiding (id, readFile)
import System.Directory qualified as D
import System.FilePath ((</>), isExtensionOf, takeBaseName)

import Relocant.DB qualified as DB (Table)
import Relocant.Migration.ID qualified as Migration (ID)
import Relocant.Migration.Interval (makeInterval_)
import Relocant.Migration.Interval qualified as Migration (Interval)
import Relocant.Migration.Name qualified as Migration (Name)


data Script = Script
  { id      :: Migration.ID
  , name    :: Migration.Name
  , content :: Content
  } deriving (Show, Eq)

instance Aeson.ToJSON Script where
  toJSON s =
    Aeson.object
      [ "id" .= s.id
      , "name" .= s.name
      , "sha1" .= show s.sha1
      ]

instance HasField "bytes" Script ByteString where
  getField m =
    m.content.bytes

instance HasField "sha1" Script (Digest SHA1) where
  getField m =
    m.content.sha1

data Content = Content
  { bytes :: ByteString
  , sha1  :: Digest SHA1
  } deriving (Show, Eq)

instance IsString Content where
  fromString str = Content
    { bytes = fromString str
    , sha1 = hash @ByteString @SHA1 (fromString str)
    }

readAll :: FilePath -> IO [Script]
readAll dir = do
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

parseFilePath :: FilePath -> (Migration.ID, Migration.Name)
parseFilePath path = do
  let
    basename =
      takeBaseName path
    (id, _rest) =
      span Char.isAlphaNum basename
  (fromString id, fromString basename)

apply :: Script -> DB.Connection -> IO Migration.Interval
apply script conn = do
  makeInterval_ (DB.execute_ conn (DB.Query script.bytes))

record :: DB.Table -> Script -> Migration.Interval -> DB.Connection -> IO ()
record table s durationS conn = do
  1 <- DB.execute conn [DB.sql|
    INSERT INTO ?
              ( id
              , name
              , bytes
              , sha1
              , applied_at
              , duration_s
              )
         SELECT ?
              , ?
              , ?
              , ?
              , CURRENT_TIMESTAMP
              , make_interval(secs := ?)
  |] ( table
     , s.id
     , s.name
     , DB.Binary s.bytes
     , DB.Binary (convert @_ @ByteString s.sha1)
     , durationS
     )
  pure ()
