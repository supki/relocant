{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Relocant.MergeSpec (spec) where

import Test.Hspec

import Relocant.Applied (Applied(..))
import Relocant.At (epoch)
import Relocant.Merge (Merged(..), ContentMismatch(..), merge)
import Relocant.Script (Script(..))


spec :: Spec
spec = do
  let
    a1 = Applied
      { id = "01"
      , name = "01-m1"
      , content = "m1"
      , appliedAt = epoch
      , durationS = mempty
      }
    a2 = Applied
      { id = "02"
      , name = "02-m2"
      , content = "m2"
      , appliedAt = epoch
      , durationS = mempty
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
