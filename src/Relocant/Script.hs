{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
  , markApplied
  ) where

import "crypton" Crypto.Hash (Digest, SHA1, hash)
import Data.Aeson qualified as Aeson
import Data.Aeson ((.=))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Char qualified as Char
import Data.List qualified as List
import Data.String (IsString(..))
import Database.PostgreSQL.Simple qualified as DB
import Database.PostgreSQL.Simple.Types qualified as DB (Query(..))
import GHC.Records (HasField(getField))
import Prelude hiding (id, readFile)
import System.Directory qualified as D
import System.FilePath ((</>), isExtensionOf, takeBaseName)

import Relocant.Applied (Applied(..))
import Relocant.At (At)
import Relocant.At qualified as At
import Relocant.ID (ID)
import Relocant.Duration (Duration)
import Relocant.Duration qualified as Duration
import Relocant.Name (Name)


data Script = Script
  { id      :: ID
  , name    :: Name
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
  getField s =
    s.content.bytes

instance HasField "sha1" Script (Digest SHA1) where
  getField s =
    s.content.sha1

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

parseFilePath :: FilePath -> (ID, Name)
parseFilePath path = do
  let
    basename =
      takeBaseName path
    (id, _rest) =
      span Char.isAlphaNum basename
  (fromString id, fromString basename)

applyWith :: (ByteString -> IO x) -> Script -> IO Applied
applyWith f s = do
  appliedAt <- At.now
  durationS <- Duration.measure_ (f s.bytes)
  pure (applied s appliedAt durationS)

apply :: Script -> DB.Connection -> IO Applied
apply s conn = do
  applyWith (DB.execute_ conn . DB.Query) s

markApplied :: Script -> IO Applied
markApplied =
  applyWith (\_bytes -> pure Duration.zeroS)

applied :: Script -> At -> Duration -> Applied
applied s appliedAt durationS = Applied
  { id = s.id
  , name = s.name
  , bytes = s.bytes
  , sha1 = s.sha1
  , appliedAt
  , durationS
  }
