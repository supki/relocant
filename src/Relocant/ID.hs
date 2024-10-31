{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK hide #-}
module Relocant.ID
  ( ID(..)
  ) where

import Data.Aeson qualified as Aeson
import Data.String (IsString)
import Database.PostgreSQL.Simple.FromField qualified as DB (FromField)
import Database.PostgreSQL.Simple.ToField qualified as DB (ToField)
import Text.Printf (PrintfArg)


-- | Migration 'ID'.
newtype ID = ID String
    deriving
      ( Show
      , Eq
      , Ord
      , IsString
      , PrintfArg
      , DB.FromField
      , DB.ToField
      , Aeson.ToJSON
      )
