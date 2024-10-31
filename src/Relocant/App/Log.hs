{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_HADDOCK hide #-}
-- | A very basic logger implementation sufficient for our needs in the CLI tool.
--
-- This logger simply writes JSON messages to stderr if their severity is higher than
-- min severity, attaching a timestamp and the running app's version.
module Relocant.App.Log
  ( Log
  , Severity(..)
  , make
  , debug
  , info
  , notice
  , warning
  , error
  ) where

import Control.Monad (when)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson (Pair)
import Data.ByteString.Lazy.Char8 qualified as ByteString.Lazy
import Data.Time (getZonedTime)
import Prelude hiding (error, log)
import System.IO qualified as IO


data Severity
  = Debug
  | Info
  | Notice
  | Warning
  | Error
    deriving (Show, Eq, Ord)

instance Aeson.ToJSON Severity where
  toJSON =
    Aeson.toJSON . \case
      Debug -> "debug"
      Info -> "info"
      Notice -> "notice"
      Warning -> "warning"
      Error -> "error" :: String

newtype Log = Log (Severity -> [Aeson.Pair] -> IO ())

make :: Severity -> Log
make minSeverity =
  Log $ \severity kvs ->
    when (severity >= minSeverity) $ do
      at <- getZonedTime
      let
        object =
          Aeson.object $
              "_at" .= at
            : "_severity" .= severity
            : kvs
      ByteString.Lazy.hPutStrLn IO.stderr (Aeson.encode object)

message :: Log -> Severity -> [Aeson.Pair] -> IO ()
message (Log f) severity =
  f severity

debug, info, notice, warning, error :: Log -> [Aeson.Pair] -> IO ()
debug log =
  message log Debug
info log =
  message log Info
notice log =
  message log Notice
warning log =
  message log Warning
error log =
  message log Error
