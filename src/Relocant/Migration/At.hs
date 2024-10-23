{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Relocant.Migration.At
  ( At
  , epoch
  ) where

import Data.Time
  ( ZonedTime
  , zonedTimeToUTC
  , utcToZonedTime
  , utc
  )
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.PostgreSQL.Simple.FromField qualified as DB (FromField)


newtype At = At ZonedTime
    deriving (Show, DB.FromField)

instance Eq At where
  At x == At y =
    zonedTimeToUTC x == zonedTimeToUTC y

epoch :: At
epoch =
  At (utcToZonedTime utc (posixSecondsToUTCTime 0))
