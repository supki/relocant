module Relocant.Migration.IntervalSpec (spec) where

import Relocant.Migration.Interval (Interval(..), makeInterval)
import Test.Hspec


spec :: Spec
spec = do
  it "times moves forward" $ do
    (Interval s, ()) <- makeInterval (pure ())
    s `shouldSatisfy` (> 0)

  it "outer > inner" $ do
    (Interval s1, (Interval s0, ())) <- makeInterval (makeInterval (pure ()))
    s1 `shouldSatisfy` (> s0)
