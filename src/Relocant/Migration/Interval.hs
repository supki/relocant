{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Relocant.Migration.Interval
  ( Interval(..)
  , makeInterval
  , makeInterval_
  ) where

import Data.Time
  ( NominalDiffTime
  , getCurrentTime
  , diffUTCTime
  )
import Database.PostgreSQL.Simple.FromField qualified as DB (FromField(..))
import Database.PostgreSQL.Simple.ToField qualified as DB (ToField(..))


newtype Interval = Interval NominalDiffTime
    deriving
      ( Show
      , Eq
      )

instance DB.FromField Interval where
  fromField conv f =
    fmap (Interval . fromIntegral @Int) (DB.fromField conv f)

instance DB.ToField Interval where
  toField (Interval s) =
    DB.toField @Int (truncate s)

makeInterval_:: IO x -> IO Interval
makeInterval_ m = do
  (t, _a) <- makeInterval m
  pure t

makeInterval :: IO a -> IO (Interval, a)
makeInterval m = do
  t0 <- getCurrentTime
  a <- m
  t1 <- getCurrentTime
  pure (Interval (t1 `diffUTCTime` t0), a)
