{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Blanks.ScopeT
  ( ScopeT (..)
  , ScopeTFold
  , hoistAnno
  , liftAnno
  , scopeTAdjointFold
  , scopeTFold
  ) where

import Blanks.Class (Blanks (..))
import Blanks.Sub (SubError (..))
import Blanks.UnderScope (BinderScope (..), BoundScope (..), EmbedScope (..), UnderScope (..), UnderScopeFold (..),
                          underScopeBind, underScopeBindOpt, underScopeFold, underScopePure, underScopeShift)
import Control.Monad (ap)
import Data.Bifoldable (bifoldr)
import Data.Bifunctor (bimap, first)
import Data.Bitraversable (bitraverse)
import Data.Functor.Adjunction (Adjunction (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

newtype ScopeT t n f a = ScopeT
  { unScopeT :: t (UnderScope n f (ScopeT t n f a) a)
  }

instance Eq (t (UnderScope n f (ScopeT t n f a) a)) => Eq (ScopeT t n f a) where
  ScopeT tu == ScopeT tv = tu == tv

instance Show (t (UnderScope n f (ScopeT t n f a) a)) => Show (ScopeT t n f a) where
  showsPrec d (ScopeT tu) = showString "ScopeT " . showsPrec (d+1) tu

liftAnno :: Functor t => t a -> ScopeT t n f a
liftAnno ta = ScopeT (fmap underScopePure ta)

hoistAnno :: (Functor t, Functor f) => (forall x. t x -> w x) -> ScopeT t n f a -> ScopeT w n f a
hoistAnno nat (ScopeT tu) = ScopeT (nat (fmap (first (hoistAnno nat)) tu))

instance (Functor t, Functor f) => Functor (ScopeT t n f) where
  fmap f (ScopeT tu) = ScopeT (fmap (bimap (fmap f) f) tu)

instance (Foldable t, Foldable f) => Foldable (ScopeT t n f) where
  foldr f z (ScopeT tu) = foldr (flip (bifoldr (flip (foldr f)) f)) z tu

instance (Traversable t, Traversable f) => Traversable (ScopeT t n f) where
  traverse f (ScopeT tu) = fmap ScopeT (traverse (bitraverse (traverse f) f) tu)

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

scopeTEmbed :: Applicative t => f (ScopeT t n f a) -> ScopeT t n f a
scopeTEmbed fs = ScopeT (pure (UnderEmbedScope (EmbedScope fs)))

scopeTCompact :: Monad t => t (ScopeT t n f a) -> ScopeT t n f a
scopeTCompact ts = ScopeT (ts >>= unScopeT)

subScopeTAbstract :: (Monad t, Functor f, Eq a) => Int -> n -> Seq a -> ScopeT t n f a -> ScopeT t n f a
subScopeTAbstract r n ks e =
  let f = fmap scopeTBound . flip Seq.elemIndexL ks
      e' = scopeTBindOpt 0 e f
  in scopeTBinder r n e'

scopeTAbstract :: (Monad t, Functor f, Eq a) => n -> Seq a -> ScopeT t n f a -> ScopeT t n f a
scopeTAbstract n ks =
  let r = Seq.length ks
  in subScopeTAbstract r n ks . scopeTShift 0 r

scopeTUnAbstract :: (Monad t, Traversable t, Functor f) => Seq a -> ScopeT t n f a -> ScopeT t n f a
scopeTUnAbstract ks = scopeTInstantiate (fmap scopeTPure ks)

subScopeTInstantiate :: (Monad t, Traversable t, Functor f) => Int -> Seq (ScopeT t n f a) -> ScopeT t n f a -> ScopeT t n f a
subScopeTInstantiate h vs = ScopeT . (>>= go h vs) . unScopeT where
  go !i ws us =
    case us of
      UnderBoundScope (BoundScope b) -> maybe (pure us) unScopeT (vs Seq.!? (b - i))
      UnderFreeScope _ -> pure us
      UnderBinderScope (BinderScope r n e) ->
        let ws' = fmap (scopeTShift 0 r) ws
            e' = subScopeTInstantiate (r + i) ws' e
        in pure (UnderBinderScope (BinderScope r n e'))
      UnderEmbedScope (EmbedScope fe) ->
        pure (UnderEmbedScope (EmbedScope (fmap (subScopeTInstantiate i ws) fe)))

scopeTInstantiate :: (Monad t, Traversable t, Functor f) => Seq (ScopeT t n f a) -> ScopeT t n f a -> ScopeT t n f a
scopeTInstantiate = subScopeTInstantiate 0

scopeTApply :: (Monad t, Traversable t, Functor f) => Seq (ScopeT t n f a) -> ScopeT t n f a -> Either SubError (ScopeT t n f a)
scopeTApply vs = fmap scopeTCompact . traverse go . unScopeT where
  go us =
    case us of
      UnderBinderScope (BinderScope r _ e) ->
        let len = Seq.length vs
        in if len == r
              then Right (scopeTShift 0 (-1) (scopeTInstantiate vs e))
              else Left (ApplyError len r)
      _ -> Left NonBinderError

type ScopeTFold t n f a r = UnderScopeFold n f (ScopeT t n f a) a r

scopeTFold :: Functor t => ScopeTFold t n f a r -> ScopeT t n f a -> t r
scopeTFold usf = fmap (underScopeFold usf) . unScopeT

scopeTAdjointFold :: Adjunction t u => ScopeTFold t n f a (u r) -> ScopeT t n f a -> r
scopeTAdjointFold usf = counit . scopeTFold usf

instance (Monad t, Traversable t, Traversable f) => Blanks n f (ScopeT t n f) where
  abstract = scopeTAbstract
  unAbstract = scopeTUnAbstract
  instantiate = scopeTInstantiate
  apply = scopeTApply
