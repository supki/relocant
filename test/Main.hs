module Main where

import Control.Exception (bracket_)
import Test.Hspec.Runner (hspec)
import Spec qualified

import SpecHelper.DB qualified as DB


main :: IO ()
main = do
  bracket_ DB.createTemplate DB.dropTemplate (hspec Spec.spec)
