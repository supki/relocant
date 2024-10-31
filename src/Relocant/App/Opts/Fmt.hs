{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_HADDOCK hide #-}
module Relocant.App.Opts.Fmt
  ( Fmt(..)
  , reader
  ) where

import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Options.Applicative (ReadM, eitherReader)


data Fmt
  = Text
  | Json
    deriving (Show, Eq)

instance Aeson.ToJSON Fmt where
  toJSON =
    Aeson.toJSON . \case
      Text -> "text" :: Text
      Json -> "json"

reader :: ReadM Fmt
reader =
  eitherReader $ \case
    "text" ->
      pure Text
    "json" ->
      pure Json
    _ ->
      Left "unknown format; possible values: text, json"
