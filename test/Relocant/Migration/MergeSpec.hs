{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Relocant.Migration.MergeSpec (spec) where

import Data.ByteString (ByteString)
import Data.String (fromString)
import "crypton" Crypto.Hash (SHA1, hash)
import Test.Hspec

import Relocant.Migration (Migration(..))
import Relocant.Migration.At (epoch)
import Relocant.Migration.Interval (zeroInterval)
import Relocant.Migration.Merge (Merged(..), ContentMismatch(..), merge)
import Relocant.Script (Script(..))


spec :: Spec
spec = do
  let
    m1 = Migration
      { id = "01"
      , name = "01-m1"
      , bytes = "m1"
      , sha1 = hash @ByteString @SHA1 (fromString "m0")
      , appliedAt = epoch
      , durationS = zeroInterval
      }
    m2 = Migration
      { id = "02"
      , name = "02-m2"
      , bytes = "m2"
      , sha1 = hash @ByteString @SHA1 (fromString "m2")
      , appliedAt = epoch
      , durationS = zeroInterval
      }
    s0 = Script
      { id = "000"
      , name = "000-m0"
      , content = "m0"
      }
    s1 = Script
      { id = "01"
      , name = "01-s1"
      , content = "s1"
      }
    s3 = Script
      { id = "03"
      , name = "03-s3"
      , content = "s3"
      }

  it "merges unapplied and applied migrations" $ do
    let
      result =
        merge [m1, m2] [s0, s1, s3]
    result `shouldBe` Merged
      { unrecorded = [s0]
      , scriptMissing = [m2]
      , contentMismatch = [ContentMismatch m1 s1]
      , unapplied = [s3]
      }
