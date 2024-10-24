{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Relocant.Migration.Interval
  ( Interval(..)
  , makeInterval
  , makeInterval_
  , zeroInterval
  ) where

import Data.Aeson qualified as Aeson
import Data.Time
  ( getCurrentTime
  , diffUTCTime
  )
import Database.PostgreSQL.Simple.FromField qualified as DB (FromField(..))
import Database.PostgreSQL.Simple.ToField qualified as DB (ToField(..))
import Text.Printf (PrintfArg)


newtype Interval = Interval Double
    deriving
      ( Show
      , Eq
      , PrintfArg
      , Aeson.ToJSON
      , DB.FromField
      , DB.ToField
      )

makeInterval_:: IO x -> IO Interval
makeInterval_ m = do
  (t, _a) <- makeInterval m
  pure t

makeInterval :: IO a -> IO (Interval, a)
makeInterval m = do
  t0 <- getCurrentTime
  a <- m
  t1 <- getCurrentTime
  pure (Interval (realToFrac (t1 `diffUTCTime` t0)), a)

zeroInterval :: Interval
zeroInterval =
  Interval 0
