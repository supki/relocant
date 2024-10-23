module Main where

import Test.Hspec.Runner (hspec)
import Spec qualified

import SpecHelper.DB qualified as DB


main :: IO ()
main = do
  DB.createTemplate
  hspec Spec.spec
