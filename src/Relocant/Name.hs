{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Relocant.Name
  ( Name
  ) where

import Data.Aeson qualified as Aeson
import Data.String (IsString)
import Database.PostgreSQL.Simple.FromField qualified as DB (FromField)
import Database.PostgreSQL.Simple.ToField qualified as DB (ToField)
import Text.Printf (PrintfArg)


newtype Name = Name String
    deriving
      ( Show
      , Eq
      , IsString
      , PrintfArg
      , Aeson.ToJSON
      , DB.FromField
      , DB.ToField
      )
