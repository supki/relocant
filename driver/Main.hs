module Main (main) where

import qualified App
import qualified Cfg


main :: IO ()
main = do
  cfg <- Cfg.get
  App.run cfg

