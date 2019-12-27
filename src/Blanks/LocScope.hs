{-# LANGUAGE UndecidableInstances #-}

module Blanks.LocScope where

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
  { unLocScope :: ScopeT n f (Located l) a
  } deriving (Functor, Foldable, Traversable, Applicative, Monad)

instance (Eq (f (ScopeT n f (Located l) a)), Eq l, Eq n, Eq a) => Eq (LocScope l n f a) where
  LocScope su == LocScope sv = su == sv
