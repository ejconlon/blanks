module Blanks.Name where

data Name n a =
  Name
    { nameKey :: n
    , nameValue :: a
    }
  deriving (Show, Functor, Foldable, Traversable)

instance Eq a => Eq (Name n a) where
  Name _ x == Name _ y = x == y

type NameOnly n = Name n ()
