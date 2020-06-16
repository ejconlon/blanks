module Blanks.Name
  ( Name (..)
  , NameOnly
  , pattern NameOnly
  ) where

-- | 'Name' is compared on value only, allowing you to define and use
-- things like 'NameOnly' in your 'BlankInfo' values to make alpha-equivalent
-- terms structurally ('Eq') equivalent.
data Name n a =
  Name
    { _nameKey :: n
    , _nameValue :: a
    }
  deriving (Show, Functor, Foldable, Traversable)

instance Eq a => Eq (Name n a) where
  Name _ x == Name _ y = x == y

-- An erased 'Name'.
type NameOnly n = Name n ()

pattern NameOnly :: n -> NameOnly n
pattern NameOnly n = Name n ()

{-# COMPLETE NameOnly #-}
