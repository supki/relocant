{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | This module deals with getting the moment of time something's happened.
module Relocant.At
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


-- | The moment of time something's happened. (This is a newtype over 'ZonedTime')
newtype At = At ZonedTime
    deriving
      ( Show
      , Aeson.ToJSON
      , DB.FromField
      , DB.ToField
      )

-- | Convert to UTC, then check equality.
instance Eq At where
  At x == At y =
    zonedTimeToUTC x == zonedTimeToUTC y

-- | Format 'At' as a 'String', using the usual /time/ format string.
format :: String -> At -> String
format fmt (At at) =
  formatTime defaultTimeLocale fmt at

-- | Get current time.
now :: IO At
now =
  fmap At getZonedTime

-- | Get '1970-01-01 00:00:00 UTC' as an 'At'.
epoch :: At
epoch =
  At (utcToZonedTime utc (posixSecondsToUTCTime 0))
