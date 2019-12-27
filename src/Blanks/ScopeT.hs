{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}

module Blanks.ScopeT where

import Blanks.UnderScope
import Control.Monad (ap)
import Data.Bifunctor (bimap, first)

newtype ScopeT n f t a = ScopeT
  { unScopeT :: t (UnderScope n f (ScopeT n f t a) a)
  }

instance Eq (t (UnderScope n f (ScopeT n f t a) a)) => Eq (ScopeT n f t a) where
  ScopeT tu == ScopeT tv = tu == tv

liftAnno :: Functor t => t a -> ScopeT n f t a
liftAnno ta = ScopeT (fmap underScopePure ta)

hoistAnno :: (Functor t, Functor f) => (forall x. t x -> w x) -> ScopeT n f t a -> ScopeT n f w a
hoistAnno nat (ScopeT tu) = ScopeT (nat (fmap (first (hoistAnno nat)) tu))

instance (Functor t, Functor f) => Functor (ScopeT n f t) where
  fmap f (ScopeT tu) = ScopeT (fmap (bimap (fmap f) f) tu)

instance (Foldable t, Foldable f) => Foldable (ScopeT n f t) where
  foldr f z (ScopeT tu) = foldr (\u x -> foldr f x u) z tu

instance (Traversable t, Traversable f) => Traversable (ScopeT n f t) where
  traverse = undefined

instance (Monad t, Traversable f) => Applicative (ScopeT n f t) where
  pure = ScopeT . pure . underScopePure
  (<*>) = ap

instance (Monad t, Traversable f) => Monad (ScopeT n f t) where
  return = pure
  (>>=) = scopeTBind 0

scopeTShift :: (Functor t, Functor f) => Int -> Int -> ScopeT n f t a -> ScopeT n f t a
scopeTShift c d (ScopeT tu) = ScopeT (fmap (\u -> underScopeShift scopeTShift c d u) tu)

subScopeTBind :: (Monad t, Traversable f) => Int -> ScopeT n f t a -> (a -> t (UnderScope n f (ScopeT n f t b) b)) -> ScopeT n f t b
subScopeTBind n (ScopeT tu) g = ScopeT (tu >>= \u -> underScopeBind scopeTShift subScopeTBind n u g)

scopeTBind :: (Monad t, Traversable f) => Int -> ScopeT n f t a -> (a -> ScopeT n f t b) -> ScopeT n f t b
scopeTBind n s f = subScopeTBind n s (unScopeT . f)
