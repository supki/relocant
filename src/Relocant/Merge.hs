{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_HADDOCK hide #-}
-- | This modules deals with merging scripts and applied migrations,
-- and discovering inconsistencies.
module Relocant.Merge
  ( Merged(..)
  , ContentMismatch(..)
  , merge
  , canApply
  , converged
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson ((.=))
import Relocant.Applied (Applied(..))
import Relocant.Script (Script(..))


-- | The result of merging 'Script's and 'Applied' migrations.
data Merged = Merged
  { unrecorded      :: [Script]
    -- ^ A script that does not have a corresponding
    -- recorded migration, when there is a recorded migration
    -- with a higher ID
  , scriptMissing   :: [Applied]
    -- ^ A recorded migration that does not have a
    -- corresponding script
  , contentMismatch :: [ContentMismatch]
    -- ^ A recorded migration and a script have the same ID but
    -- different content
  , unapplied       :: [Script]
    -- ^ An unapplied script that has a higher ID than any
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

-- | The applied migration and its purported script differ in content.
data ContentMismatch = ContentMismatch
  { expected :: Applied
  , butGot   :: Script
  } deriving (Show, Eq)

instance Aeson.ToJSON ContentMismatch where
  toJSON cm =
    Aeson.object
       [ "expected" .= cm.expected
       , "but-got" .= cm.butGot
       ]

-- | No problems have been discovered after the merge.
canApply :: Merged -> Bool
canApply = \case
  Merged {unrecorded = [], scriptMissing = [], contentMismatch = []} -> True
  _ -> False

-- | No problems have been discovered after the merge, and there are no migration scripts to apply.
converged :: Merged -> Bool
converged = \case
  Merged {unrecorded = [], scriptMissing = [], contentMismatch = [], unapplied = []} -> True
  _ -> False

-- | Merge scripts and applied migrations, trying to discover inconsistencies and/or
-- migration scripts to apply.
merge :: [Applied] -> [Script] -> Merged
merge applieds scripts =
  fromAcc (go ([], [], []) applieds scripts)
 where
  go
    :: ([Script], [Applied], [ContentMismatch])
    -> [Applied]
    -> [Script]
    -> ([Script], [Applied], [ContentMismatch], [Script])
  go (unrecorded, scriptMissing, contentMismatch) [] ss =
    (unrecorded, scriptMissing, contentMismatch, ss)
  go (unrecorded, scriptMissing, contentMismatch) as [] =
    (unrecorded, scriptMissing <> as, contentMismatch, [])
  go (unrecorded, scriptMissing, contentMismatch) (a : as) (s : ss) =
    case compare a.id s.id of
      LT ->
        go (unrecorded, a : scriptMissing, contentMismatch) as (s : ss)
      EQ
        | a.checksum == s.checksum ->
          go (unrecorded, scriptMissing, contentMismatch) as ss
        | otherwise ->
          go (unrecorded, scriptMissing, ContentMismatch a s : contentMismatch) as ss
      GT ->
        go (s : unrecorded, scriptMissing, contentMismatch) (a : as) ss

  fromAcc (unrecorded, unscripted, contentMismatch, unapplied) = Merged
    { unrecorded = reverse unrecorded
    , scriptMissing = reverse unscripted
    , contentMismatch = reverse contentMismatch
    , unapplied
    }
