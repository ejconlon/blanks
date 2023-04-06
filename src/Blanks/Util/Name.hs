{-# LANGUAGE DeriveAnyClass #-}

module Blanks.Util.Name
  ( Name (..)
  , NameOnly
  , pattern NameOnly
  )
where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- | 'Name' is compared on value only, allowing you to define and use
-- things like 'NameOnly' in your 'BlankInfo' values to make alpha-equivalent
-- terms structurally ('Eq') equivalent.
-- If you define your own info type, you may want to define 'Eq' to work in a similar way.
data Name n a = Name
  { nameKey :: !n
  , nameValue :: !a
  }
  deriving stock (Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData)

instance Eq a => Eq (Name n a) where
  Name _ x == Name _ y = x == y

-- An erased 'Name'.
type NameOnly n = Name n ()

pattern NameOnly :: n -> NameOnly n
pattern NameOnly n = Name n ()

{-# COMPLETE NameOnly #-}
