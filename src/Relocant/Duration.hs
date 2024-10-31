{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
-- | This module deals with timing actions.
module Relocant.Duration
  ( Duration(..)
  , measure
  , measure_
  ) where

import Data.Aeson qualified as Aeson
import Data.Time
  ( getCurrentTime
  , diffUTCTime
  )
import Database.PostgreSQL.Simple.FromField qualified as DB (FromField(..))
import Database.PostgreSQL.Simple.ToField qualified as DB (ToField(..))
import Text.Printf (PrintfArg)


-- | How long something took, in seconds. Use 'mempty' for a 0 seconds 'Duration'.
newtype Duration = Duration Double
    deriving
      ( Show
      , Eq
      , PrintfArg
      , Aeson.ToJSON
      , DB.FromField
      , DB.ToField
      )

instance Semigroup Duration where
  (Duration d0) <> (Duration d1) =
    Duration (d0 + d1)

instance Monoid Duration where
  mempty = Duration 0

-- | Time an action.
measure :: IO a -> IO (Duration, a)
measure m = do
  t0 <- getCurrentTime
  a <- m
  t1 <- getCurrentTime
  pure (Duration (realToFrac (t1 `diffUTCTime` t0)), a)

-- | Time an action and ignore its result.
measure_ :: IO x -> IO Duration
measure_ m = do
  (t, _x) <- measure m
  pure t
