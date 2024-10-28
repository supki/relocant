module Relocant.Migration.DurationSpec (spec) where

import Test.Hspec

import Relocant.Migration.Duration (Duration(..), measure)


spec :: Spec
spec = do
  it "times moves forward" $ do
    (Duration s, ()) <- measure (pure ())
    s `shouldSatisfy` (> 0)

  it "outer > inner" $ do
    (Duration s1, (Duration s0, ())) <- measure (measure (pure ()))
    s1 `shouldSatisfy` (> s0)
