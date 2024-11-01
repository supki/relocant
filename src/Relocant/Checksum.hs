{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK hide #-}
module Relocant.Checksum
  ( Checksum(..)
  ) where

import Control.Applicative (empty)
import "crypton" Crypto.Hash (Digest, SHA1, hash, digestFromByteString)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.String (IsString(..))
import Database.PostgreSQL.Simple qualified as DB (Binary(..))
import Database.PostgreSQL.Simple.FromField qualified as DB (FromField(..))
import Database.PostgreSQL.Simple.ToField qualified as DB (ToField(..))


-- | SHA1 checksum.
newtype Checksum = Checksum (Digest SHA1)
    deriving Show via Digest SHA1 -- just show the digest
    deriving Eq

instance IsString Checksum where
  fromString str =
    Checksum (hash @ByteString @SHA1 (fromString str))

instance DB.FromField Checksum where
  fromField conv f = do
    DB.Binary bytes <- DB.fromField conv f
    case digestFromByteString @_ @ByteString bytes of
      Nothing ->
        empty
      Just digest ->
        pure (Checksum digest)

instance DB.ToField Checksum where
  toField (Checksum digest) = do
    DB.toField (DB.Binary (convert @_ @ByteString digest))
