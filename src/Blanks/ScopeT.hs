{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}

module Blanks.ScopeT where

import Blanks.Class (Blanks (..))
import Blanks.UnderScope (UnderScope, underScopeBind, underScopePure, underScopeShift)
import Control.Monad (ap)
import Data.Bifunctor (bimap, first)

newtype ScopeT t n f a = ScopeT
  { unScopeT :: t (UnderScope n f (ScopeT t n f a) a)
  }

instance Eq (t (UnderScope n f (ScopeT t n f a) a)) => Eq (ScopeT t n f a) where
  ScopeT tu == ScopeT tv = tu == tv

liftAnno :: Functor t => t a -> ScopeT t n f a
liftAnno ta = ScopeT (fmap underScopePure ta)

hoistAnno :: (Functor t, Functor f) => (forall x. t x -> w x) -> ScopeT t n f a -> ScopeT w n f a
hoistAnno nat (ScopeT tu) = ScopeT (nat (fmap (first (hoistAnno nat)) tu))

instance (Functor t, Functor f) => Functor (ScopeT t n f) where
  fmap f (ScopeT tu) = ScopeT (fmap (bimap (fmap f) f) tu)

instance (Foldable t, Foldable f) => Foldable (ScopeT t n f) where
  foldr f z (ScopeT tu) = foldr (\u x -> foldr f x u) z tu

instance (Traversable t, Traversable f) => Traversable (ScopeT t n f) where
  traverse = undefined

scopeTPure :: Applicative t => a -> ScopeT t n f a
scopeTPure = ScopeT . pure . underScopePure

instance (Monad t, Traversable f) => Applicative (ScopeT t n f) where
  pure = scopeTPure
  (<*>) = ap

instance (Monad t, Traversable f) => Monad (ScopeT t n f) where
  return = pure
  (>>=) = scopeTBind 0

scopeTShift :: (Functor t, Functor f) => Int -> Int -> ScopeT t n f a -> ScopeT t n f a
scopeTShift c d (ScopeT tu) = ScopeT (fmap (\u -> underScopeShift scopeTShift c d u) tu)

subScopeTBind :: (Monad t, Traversable f) => Int -> ScopeT t n f a -> (a -> t (UnderScope n f (ScopeT t n f b) b)) -> ScopeT t n f b
subScopeTBind n (ScopeT tu) g = ScopeT (tu >>= \u -> underScopeBind scopeTShift subScopeTBind n u g)

scopeTBind :: (Monad t, Traversable f) => Int -> ScopeT t n f a -> (a -> ScopeT t n f b) -> ScopeT t n f b
scopeTBind n s f = subScopeTBind n s (unScopeT . f)

instance Monad t => Blanks (ScopeT t) where
  pureBlanks = scopeTPure
  bindBlanks = scopeTBind 0
