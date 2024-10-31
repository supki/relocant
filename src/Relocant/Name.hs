{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK hide #-}
module Relocant.Name
  ( Name(..)
  ) where

import Data.Aeson qualified as Aeson
import Data.String (IsString)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField qualified as DB (FromField)
import Database.PostgreSQL.Simple.ToField qualified as DB (ToField)
import Text.Printf (PrintfArg)


-- | Migration 'Name'.
newtype Name = Name Text
    deriving
      ( Show
      , Eq
      , IsString
      , PrintfArg
      , Aeson.ToJSON
      , DB.FromField
      , DB.ToField
      )
