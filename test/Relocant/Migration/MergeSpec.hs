{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Relocant.Migration.MergeSpec (spec) where

import Data.ByteString (ByteString)
import Data.String (fromString)
import "crypton" Crypto.Hash (SHA1, hash)
import Test.Hspec

import Relocant.Migration.Applied (Applied(..))
import Relocant.Migration.At (epoch)
import Relocant.Migration.Duration (zeroS)
import Relocant.Migration.Merge (Merged(..), ContentMismatch(..), merge)
import Relocant.Script (Script(..))


spec :: Spec
spec = do
  let
    a1 = Applied
      { id = "01"
      , name = "01-m1"
      , bytes = "m1"
      , sha1 = hash @ByteString @SHA1 (fromString "m0")
      , appliedAt = epoch
      , durationS = zeroS
      }
    a2 = Applied
      { id = "02"
      , name = "02-m2"
      , bytes = "m2"
      , sha1 = hash @ByteString @SHA1 (fromString "m2")
      , appliedAt = epoch
      , durationS = zeroS
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
        merge [a1, a2] [s0, s1, s3]
    result `shouldBe` Merged
      { unrecorded = [s0]
      , scriptMissing = [a2]
      , contentMismatch = [ContentMismatch a1 s1]
      , unapplied = [s3]
      }
