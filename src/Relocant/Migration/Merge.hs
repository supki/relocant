{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Relocant.Migration.Merge
  ( Merged(..)
  , ContentMismatch(..)
  , merge
  , canApply
  , converged
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson ((.=))
import Relocant.Migration (Migration(..))
import Relocant.Script (Script(..))


data Merged = Merged
  { unrecorded      :: [Script]
    -- ^ a script that does not have a corresponding
    -- recorded migration, when there is a recorded migration
    -- with a higher ID
  , scriptMissing   :: [Migration]
    -- ^ a recorded migration that does not have a
    -- corresponding script
  , contentMismatch :: [ContentMismatch]
    -- ^ a recorded migration and a script have the same ID but
    -- different content
  , unapplied       :: [Script]
    -- ^ an unapplied script that has a higher ID than any
    -- recorded migration
  } deriving (Show, Eq)

instance Aeson.ToJSON Merged where
  toJSON r =
    Aeson.object
       [ "unrecorded" .= r.unrecorded
       , "script-missing" .= r.scriptMissing
       , "content-mismatch" .= r.contentMismatch
       , "unapplied" .= r.unapplied
       ]

data ContentMismatch = ContentMismatch
  { expected :: Migration
  , butGot   :: Script
  } deriving (Show, Eq)

instance Aeson.ToJSON ContentMismatch where
  toJSON cm =
    Aeson.object
       [ "expected" .= cm.expected
       , "but-got" .= cm.butGot
       ]

canApply :: Merged -> Bool
canApply = \case
  Merged {unrecorded = [], scriptMissing = [], contentMismatch = []} -> True
  _ -> False

converged :: Merged -> Bool
converged = \case
  Merged {unrecorded = [], scriptMissing = [], contentMismatch = [], unapplied = []} -> True
  _ -> False

merge :: [Migration] -> [Script] -> Merged
merge migrations scripts =
  fromAcc (go ([], [], []) migrations scripts)
 where
  go
    :: ([Script], [Migration], [ContentMismatch])
    -> [Migration]
    -> [Script]
    -> ([Script], [Migration], [ContentMismatch], [Script])
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
          go (unrecorded, scriptMissing, ContentMismatch m s : contentMismatch) ms ss
      GT ->
        go (s : unrecorded, scriptMissing, contentMismatch) (m : ms) ss

  fromAcc (unrecorded, unscripted, contentMismatch, unapplied) = Merged
    { unrecorded = reverse unrecorded
    , scriptMissing = reverse unscripted
    , contentMismatch = reverse contentMismatch
    , unapplied
    }
