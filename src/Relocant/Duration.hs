{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Relocant.Duration
  ( Duration(..)
  , measure
  , measure_
  , zeroS
  ) where

import Data.Aeson qualified as Aeson
import Data.Time
  ( getCurrentTime
  , diffUTCTime
  )
import Database.PostgreSQL.Simple.FromField qualified as DB (FromField(..))
import Database.PostgreSQL.Simple.ToField qualified as DB (ToField(..))
import Text.Printf (PrintfArg)


newtype Duration = Duration Double
    deriving
      ( Show
      , Eq
      , PrintfArg
      , Aeson.ToJSON
      , DB.FromField
      , DB.ToField
      )

measure :: IO a -> IO (Duration, a)
measure m = do
  t0 <- getCurrentTime
  a <- m
  t1 <- getCurrentTime
  pure (Duration (realToFrac (t1 `diffUTCTime` t0)), a)

measure_ :: IO x -> IO Duration
measure_ m = do
  (t, _x) <- measure m
  pure t

zeroS :: Duration
zeroS =
  Duration 0
