{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Relocant.Migration.Merge
  ( Result(..)
  , merge
  , ready
  , converged
  ) where

import Relocant.Migration (Migration(..))
import Relocant.Script (Script(..))


data Result = Result
  { unrecorded      :: [Script]
    -- ^ a script that does not have a corresponding
    -- recorded migration, when there is a recorded migration
    -- with a higher ID
  , scriptMissing   :: [Migration]
    -- ^ a recorded migration that does not have a
    -- corresponding script
  , contentMismatch :: [(Migration, Script)]
    -- ^ a recorded migration and a script have the same ID but
    -- different content
  , unapplied       :: [Script]
    -- ^ an unapplied script that has a higher ID than any
    -- recorded migration
  } deriving (Show, Eq)

ready :: Result -> Bool
ready = \case
  Result {unrecorded = [], scriptMissing = [], contentMismatch = []} -> True
  _ -> False

converged :: Result -> Bool
converged = \case
  Result {unrecorded = [], scriptMissing = [], contentMismatch = [], unapplied = []} -> True
  _ -> False

merge :: [Migration] -> [Script] -> Result
merge migrations scripts =
  fromAcc (go ([], [], []) migrations scripts)
 where
  go
    :: ([Script], [Migration], [(Migration, Script)])
    -> [Migration]
    -> [Script]
    -> ([Script], [Migration], [(Migration, Script)], [Script])
  go (unrecorded, scriptMissing, contentMismatch) [] ss =
    (unrecorded, scriptMissing, contentMismatch, ss)
  go (unrecorded, scriptMissing, contentMismatch) ms [] =
    (unrecorded, scriptMissing <> ms, contentMismatch, [])
  go (unrecorded, scriptMissing, contentMismatch) (m : ms) (s : ss) =
    case compare m.id s.id of
      LT ->
        go (unrecorded, m : scriptMissing, contentMismatch) ms (s : ss)
      EQ
        | s.sha1 == m.sha1 ->
          go (unrecorded, scriptMissing, contentMismatch) ms ss
        | otherwise ->
          go (unrecorded, scriptMissing, (m, s) : contentMismatch) ms ss
      GT ->
        go (s : unrecorded, scriptMissing, contentMismatch) (m : ms) ss

  fromAcc (unrecorded, unscripted, contentMismatch, unapplied) = Result
    { unrecorded = reverse unrecorded
    , scriptMissing = reverse unscripted
    , contentMismatch = reverse contentMismatch
    , unapplied
    }
