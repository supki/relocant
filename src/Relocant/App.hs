module Relocant.App
  ( run
  ) where

import Data.Foldable (traverse_)

import Relocant.App.Opts qualified as Opts
import Relocant.Script qualified as Script


run :: IO ()
run = do
  cmd <- Opts.parse
  case cmd of
    Opts.ListScripts dir -> do
      scripts <- Script.listDirectory dir
      traverse_ print scripts
