{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
-- | This module deals with migrations that haven't yet been applied.
module Relocant.Script
  ( Script(..)
  , readScripts
  , readScript
  , apply
  , markApplied
    -- * Extra
  , parseFilePath
  , readContent
  ) where

import Control.Exception (catch, throwIO)
import "crypton" Crypto.Hash (hash)
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
import Prelude hiding (id)
import System.Directory qualified as D
import System.FilePath ((</>), isExtensionOf, takeBaseName)

import Relocant.Applied (Applied(..))
import Relocant.At (At)
import Relocant.At qualified as At
import Relocant.ID (ID)
import Relocant.Checksum (Checksum(..))
import Relocant.Content (Content(..))
import Relocant.Duration (Duration)
import Relocant.Duration qualified as Duration
import Relocant.Name (Name)


-- | A migration script. Most users would get them by running 'readScripts',
-- but those wanting a more fine-grained control would use 'readScript'.
data Script = Script
  { id      :: ID      -- ^ if XXX-YYYY.sql is the whole filename, then XXX is the id; see 'parseFilePath'
  , name    :: Name    -- ^ if XXX-YYYY.sql is the whole filename, then XXX-YYYY is the name
  , content :: Content
  } deriving (Show, Eq)

-- | The content (as in actual bytes) is skipped.
instance Aeson.ToJSON Script where
  toJSON s =
    Aeson.object
      [ "id" .= s.id
      , "name" .= s.name
      , "checksum" .= show s.checksum
      ]

instance HasField "bytes" Script ByteString where
  getField s =
    s.content.bytes

instance HasField "checksum" Script Checksum where
  getField s =
    s.content.checksum

-- | Read migration 'Script's from a directory. It only tries to read paths
-- ending with the /.sql/ extension and doesn't recurse into subdirectories.
--
-- /Note/: the directory must exist
readScripts :: FilePath -> IO [Script]
readScripts dir = do
  paths <- D.listDirectory dir
  scripts <- traverse (\path -> readScript (dir </> path)) (filter (isExtensionOf ".sql") paths)
  pure (List.sortOn (\script -> script.id) scripts)

-- | Read migration 'Script's from a file. Useful when you need a more fine-grained
-- control over which paths are read then what 'readScripts' provides.
readScript :: FilePath -> IO Script
readScript path = do
  let
    (id, name) =
      parseFilePath path
  content <- readContent path
  pure Script
    { id
    , name
    , content
    }

-- | Get XXX as the 'ID' and XXX-YYYY as the 'Name' from XXX-YYYY.sql
--
-- Basically, the longest alphanumeric prefix of the basename is the 'ID',
-- and the basename is the 'Name'.
parseFilePath :: FilePath -> (ID, Name)
parseFilePath path = do
  let
    basename =
      takeBaseName path
    (id, _rest) =
      span Char.isAlphaNum basename
  (fromString id, fromString basename)

-- | Get migration 'Script'\'s content and its checksum.
readContent :: FilePath -> IO Content
readContent path = do
  bytes <- ByteString.readFile path
  pure Content
    { bytes
    , checksum = Checksum (hash bytes)
    }

applyWith :: (ByteString -> IO Duration) -> Script -> IO Applied
applyWith f s = do
  appliedAt <- At.now
  durationS <- f s.bytes
  pure (applied s appliedAt durationS)

-- | Run a 'Script' against the database, get an 'Applied' migration back if successful.
--
-- /Note:/ You probably want to run this together with 'Relocant.Applied.record'
-- in a single transaction.
-- /Note:/ This has to run inside a transaction context, will error out otherwise.
apply :: Script -> DB.Connection -> IO Applied
apply s conn = do
  applyWith (Duration.measure_ . execute_ conn . DB.Query) s

-- Like DB.execute_, but ignores empty query errors.
execute_ :: DB.Connection -> DB.Query -> IO ()
execute_ conn query = do
  _ <- DB.withSavepoint conn (DB.execute_ conn query)
  pure ()
 `catch`
  \exc ->
    case exc of
      DB.QueryError {DB.qeMessage = "execute: Empty query"} ->
        pure ()
      _ ->
        throwIO exc

-- | Get an 'Applied' migration from a 'Script', without running it. This should not
-- be normally used, but can be useful for fixing checksums after cosmetics updates
-- of migration 'Script's (such as adding comments, for example).
markApplied :: Script -> IO Applied
markApplied =
  applyWith (\_bytes -> pure mempty)

applied :: Script -> At -> Duration -> Applied
applied s appliedAt durationS = Applied
  { id = s.id
  , name = s.name
  , content = s.content
  , appliedAt
  , durationS
  }
