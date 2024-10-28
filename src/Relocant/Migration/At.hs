{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Relocant.Migration.At
  ( At
  , format
  , now
  , epoch
  ) where

import Data.Aeson qualified as Aeson
import Data.Time
  ( ZonedTime
  , formatTime
  , defaultTimeLocale
  , getZonedTime
  , zonedTimeToUTC
  , utcToZonedTime
  , utc
  )
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.PostgreSQL.Simple.FromField qualified as DB (FromField)
import Database.PostgreSQL.Simple.ToField qualified as DB (ToField)


newtype At = At ZonedTime
    deriving
      ( Show
      , Aeson.ToJSON
      , DB.FromField
      , DB.ToField
      )

instance Eq At where
  At x == At y =
    zonedTimeToUTC x == zonedTimeToUTC y

format :: String -> At -> String
format fmt (At at) =
  formatTime defaultTimeLocale fmt at

now :: IO At
now =
  fmap At getZonedTime

epoch :: At
epoch =
  At (utcToZonedTime utc (posixSecondsToUTCTime 0))
