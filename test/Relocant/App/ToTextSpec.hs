module Relocant.App.ToTextSpec where

import Data.Text qualified as Text
import Test.Hspec

import Relocant.Name (Name(..))
import Relocant.App.ToText
  ( ToText(..)
  , arbitraryNameLengthCutOff
  )


spec :: Spec
spec =
  describe "Name" $ do
    it "length smaller than arbitraryNameLengthCutOff" $ do
      let
        name =
          Text.replicate 7 "a"
      toText (Name name) `shouldBe`
        name <> Text.replicate (arbitraryNameLengthCutOff - 7) " "

    it "length exactly the arbitraryNameLengthCutOff" $ do
      let
        name =
          Text.replicate arbitraryNameLengthCutOff "a"
      toText (Name name) `shouldBe`
        name

    it "length larger than arbitraryNameLengthCutOff" $ do
      let
        name =
          Text.replicate (arbitraryNameLengthCutOff + 5) "a"
      toText (Name name) `shouldBe`
        Text.take 19 name <> "…"
