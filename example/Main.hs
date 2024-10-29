{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (unless)
import Data.Foldable (for_)
import System.Exit (exitFailure)

import Relocant qualified


main :: IO ()
main = do
  let
    table = Relocant.defaultTable
  conn <- Relocant.connect "dbname=relocant" table
  -- lock the db, so that multiple relocant users can run concurrently
  Relocant.withLock table conn $ do
    -- grab all data and look for irregularities
    merged0 <- Relocant.mergeAll table conn "./migration"

    -- exit immediatelly if any problems have been detected
    unless (Relocant.canApply merged0) exitFailure

    for_ merged0.unapplied $ \script ->
      -- run a separate transaction for each unapplied migration script
      Relocant.withTransaction conn $ do
        applied <- Relocant.apply script conn
        Relocant.record applied table conn

    -- grab all data again, the scripts and applied migrations should've converged
    merged1 <- Relocant.mergeAll table conn "./migration"

    -- fail if they haven't
    unless (Relocant.converged merged1) exitFailure