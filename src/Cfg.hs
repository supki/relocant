module Cfg
  ( Cfg(..)
  , get
  , Meta.name
  , Meta.version
  ) where

import           Env
import qualified Meta_relocant as Meta


data Cfg = Cfg
    deriving (Show, Eq)

get :: IO Cfg
get =
  Env.parse (header usageHeader) . prefixed "RELOCANT_" $ do
    pure Cfg

usageHeader :: String
usageHeader =
  unwords [Meta.name, Meta.version]

