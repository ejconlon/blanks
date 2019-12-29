{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}

module Blanks.ScopeT
  ( ScopeT (..)
  , liftAnno
  , hoistAnno
  ) where

import Blanks.Class (Blanks (..))
import Blanks.UnderScope (BinderScope (..), BoundScope (..), UnderScope (..), underScopeBind, underScopeBindOpt,
                          underScopePure, underScopeShift)
import Control.Monad (ap)
import Data.Bifunctor (bimap, first)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

newtype ScopeT t n f a = ScopeT
  { unScopeT :: t (UnderScope n f (ScopeT t n f a) a)
  }

liftUnder :: Functor t => (UnderScope n f (ScopeT t n f a) a -> UnderScope n f (ScopeT t n f b) b) -> ScopeT t n f a -> ScopeT t n f b
liftUnder f = ScopeT . fmap f . unScopeT

instance Eq (t (UnderScope n f (ScopeT t n f a) a)) => Eq (ScopeT t n f a) where
  ScopeT tu == ScopeT tv = tu == tv

liftAnno :: Functor t => t a -> ScopeT t n f a
liftAnno ta = ScopeT (fmap underScopePure ta)

hoistAnno :: (Functor t, Functor f) => (forall x. t x -> w x) -> ScopeT t n f a -> ScopeT w n f a
hoistAnno nat (ScopeT tu) = ScopeT (nat (fmap (first (hoistAnno nat)) tu))

instance (Functor t, Functor f) => Functor (ScopeT t n f) where
  fmap f (ScopeT tu) = ScopeT (fmap (bimap (fmap f) f) tu)

instance (Foldable t, Foldable f) => Foldable (ScopeT t n f) where
  foldr f z (ScopeT tu) = foldr (flip (foldr f)) z tu

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
scopeTShift c d (ScopeT tu) = ScopeT (fmap (underScopeShift scopeTShift c d) tu)

subScopeTBind :: (Monad t, Functor f) => Int -> ScopeT t n f a -> (a -> t (UnderScope n f (ScopeT t n f b) b)) -> ScopeT t n f b
subScopeTBind n (ScopeT tu) g = ScopeT (tu >>= \u -> underScopeBind scopeTShift subScopeTBind n u g)

scopeTBind :: (Monad t, Functor f) => Int -> ScopeT t n f a -> (a -> ScopeT t n f b) -> ScopeT t n f b
scopeTBind n s f = subScopeTBind n s (unScopeT . f)

subScopeTBindOpt :: (Monad t, Functor f) => Int -> ScopeT t n f a -> (a -> Maybe (t (UnderScope n f (ScopeT t n f a) a))) -> ScopeT t n f a
subScopeTBindOpt n (ScopeT tu) g = ScopeT (tu >>= \u -> underScopeBindOpt scopeTShift subScopeTBindOpt n u g)

scopeTBindOpt :: (Monad t, Functor f) => Int -> ScopeT t n f a -> (a -> Maybe (ScopeT t n f a)) -> ScopeT t n f a
scopeTBindOpt n s f = subScopeTBindOpt n s (fmap unScopeT . f)

scopeTBound :: Applicative t => Int -> ScopeT t n f a
scopeTBound = ScopeT . pure . UnderBoundScope . BoundScope

scopeTBinder :: Applicative t => Int -> n -> ScopeT t n f a -> ScopeT t n f a
scopeTBinder r n e = ScopeT (pure (UnderBinderScope (BinderScope r n e)))

subScopeTAbstract :: (Monad t, Functor f, Eq a) => Int -> n -> Seq a -> ScopeT t n f a -> ScopeT t n f a
subScopeTAbstract r n ks e =
  let f = fmap scopeTBound . flip Seq.elemIndexL ks
      e' = scopeTBindOpt 0 e f
  in scopeTBinder r n e'

scopeTAbstract :: (Monad t, Functor f, Eq a) => n -> Seq a -> ScopeT t n f a -> ScopeT t n f a
scopeTAbstract n ks =
  let r = Seq.length ks
  in subScopeTAbstract r n ks . scopeTShift 0 r

instance (Monad t, Traversable f) => Blanks n f (ScopeT t n f) where
  abstract = scopeTAbstract
