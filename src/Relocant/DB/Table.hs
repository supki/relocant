{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Relocant.DB.Table
  ( Table
  , defaultTable
  ) where

import Data.String (IsString)
import Database.PostgreSQL.Simple.ToField qualified as DB (ToField)
import Database.PostgreSQL.Simple.Types qualified as DB (QualifiedIdentifier)


newtype Table = Table DB.QualifiedIdentifier
    deriving
      ( Show
      , Eq
      , IsString
      , DB.ToField
      )

defaultTable :: Table
defaultTable =
  "public.relocant_migration"
