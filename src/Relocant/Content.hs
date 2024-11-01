{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK hide #-}
module Relocant.Content where

import Data.ByteString (ByteString)
import Data.String (IsString(..))

import Relocant.Checksum (Checksum)


-- | Migration content and its checksum.
data Content = Content
  { bytes    :: ByteString
  , checksum :: Checksum
  } deriving (Show, Eq)

instance IsString Content where
  fromString str = Content
    { bytes = fromString str
    , checksum = fromString str
    }
