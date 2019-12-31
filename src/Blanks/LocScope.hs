{-# LANGUAGE UndecidableInstances #-}

module Blanks.LocScope
  ( Located (..)
  , LocScope (..)
  , LocScopeFold
  , locScopeFold
  ) where

import Blanks.Class (Blanks (..))
import Blanks.ScopeT
import Control.Monad (ap)

data Located l a = Located
  { _locatedLoc :: l
  , _locatedVal :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance Monoid l => Applicative (Located l) where
  pure = Located mempty
  (<*>) = ap

instance Monoid l => Monad (Located l) where
  return = pure
  Located l a >>= f = let Located p b = f a in Located (l <> p) b

newtype LocScope l n f a = LocScope
  { unLocScope :: ScopeT (Located l) n f a
  } deriving (Functor, Foldable, Traversable, Applicative, Monad, Blanks n f)

instance (Eq (f (ScopeT (Located l) n f a)), Eq l, Eq n, Eq a) => Eq (LocScope l n f a) where
  LocScope su == LocScope sv = su == sv

instance (Show (f (ScopeT (Located l) n f a)), Show l, Show n, Show a) => Show (LocScope l n f a) where
  showsPrec d (LocScope (ScopeT tu)) = showString "LocScope " . showsPrec (d+1) tu

type LocScopeFold l n f a r = BlankFold (LocScope l n f) a r

locScopeFold :: LocScopeFold l n f a r -> LocScope l n f a -> r
locScopeFold = undefined
