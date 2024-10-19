{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Relocant.Migration.Interval
  ( Interval
  , makeInterval
  , makeInterval_
  ) where

import Data.Time
  ( getCurrentTime
  , diffUTCTime
  )
import Database.PostgreSQL.Simple.FromField qualified as DB (FromField)
import Database.PostgreSQL.Simple.ToField qualified as DB (ToField)


newtype Interval = Interval Int
    deriving
      ( Show
      , Eq
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
  pure (Interval (truncate (t1 `diffUTCTime` t0)), a)
