module Relocant.App.ToText where

import Control.Monad (guard)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Printf (printf)

import Relocant.Applied (Applied(..))
import Relocant.At qualified as At
import Relocant.Merge (Merged(..), ContentMismatch(..))
import Relocant.Script (Script(..))


class ToText t where
  toText :: t -> Text

instance ToText Script where
  toText s =
    fromString (printf "%s\t%s\t%s" s.id s.name (take 8 (show s.sha1)))

instance ToText Applied where
  toText a =
    fromString
      (printf "%s\t%s\t%s\t%s\t%.2fs"
        a.id
        a.name
        (take 8 (show a.sha1))
        (At.format "%F %T %z" a.appliedAt)
        a.durationS)

instance ToText Merged where
  toText r =
    Text.intercalate "\n" $ concat
      [ do guard (not (null r.unrecorded)); "unrecorded:" : map toText r.unrecorded
      , do guard (not (null r.scriptMissing)); "script missing:" : map toText r.scriptMissing
      , do guard (not (null r.contentMismatch)); "content mismatch:" : map toText r.contentMismatch
      , do guard (not (null r.unapplied)); "unapplied:" : map toText r.unapplied
      ]

instance ToText ContentMismatch where
  toText cm =
    fromString (printf "expected: %s\n but got: %s" (toText cm.expected) (toText cm.butGot))
