{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Relocant.Migration.Name
  ( Name
  ) where

import Data.String (IsString)
import Database.PostgreSQL.Simple.FromField qualified as DB (FromField)
import Database.PostgreSQL.Simple.ToField qualified as DB (ToField)


newtype Name = Name String
    deriving
      ( Show
      , Eq
      , IsString
      , DB.FromField
      , DB.ToField
      )
