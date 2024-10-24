{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Relocant.Migration.At
  ( At
  , epoch
  , format
  ) where

import Data.Aeson qualified as Aeson
import Data.Time
  ( ZonedTime
  , zonedTimeToUTC
  , utcToZonedTime
  , utc
  , formatTime
  , defaultTimeLocale
  )
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.PostgreSQL.Simple.FromField qualified as DB (FromField)


newtype At = At ZonedTime
    deriving
      ( Show
      , Aeson.ToJSON
      , DB.FromField
      )

instance Eq At where
  At x == At y =
    zonedTimeToUTC x == zonedTimeToUTC y

format :: String -> At -> String
format fmt (At at) =
  formatTime defaultTimeLocale fmt at

epoch :: At
epoch =
  At (utcToZonedTime utc (posixSecondsToUTCTime 0))
