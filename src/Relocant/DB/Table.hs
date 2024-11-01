{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK hide #-}
module Relocant.DB.Table
  ( Table(..)
  , defaultTable
  ) where

import Data.Aeson qualified as Aeson
import Data.String (IsString(..))
import Database.PostgreSQL.Simple.ToField qualified as DB (ToField)
import Database.PostgreSQL.Simple.Types qualified as DB (QualifiedIdentifier(..))
import Text.Printf (printf)


-- | /relocant's/ migration table name.
newtype Table = Table DB.QualifiedIdentifier
    deriving
      ( Eq
      , IsString
      , DB.ToField
      )

-- | Return a 'String' that can be 'fromString'ed back to a 'Table'.
instance Show Table where
  show (Table (DB.QualifiedIdentifier q i)) =
    case q of
      Just s ->
        printf "%s.%s" s i
      Nothing ->
        printf "%s" i

instance Aeson.ToJSON Table where
  toJSON =
    Aeson.toJSON . show

-- | The default table name, which is "public.relocant_migration".
defaultTable :: Table
defaultTable =
  "public.relocant_migration"
