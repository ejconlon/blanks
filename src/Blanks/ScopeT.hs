{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Blanks.ScopeT
  ( ScopeT (..)
  , ScopeTFold
  , scopeTAdjointFold
  , scopeTHoistAnno
  , scopeTBind
  , scopeTFree
  , scopeTFold
  , scopeTLiftAnno
  ) where

import Blanks.Class
import Blanks.Sub (SubError (..))
import Blanks.UnderScope (BinderScope (..), BoundScope (..), EmbedScope (..), FreeScope (..), UnderScope (..),
                          UnderScopeFold (..), underScopeBind, underScopeBindOpt, underScopeFold, underScopePure,
                          underScopeShift)
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

instance (Functor t, Functor f) => Functor (ScopeT t n f) where
  fmap f (ScopeT tu) = ScopeT (fmap (bimap (fmap f) f) tu)

instance (Foldable t, Foldable f) => Foldable (ScopeT t n f) where
  foldr f z (ScopeT tu) = foldr (flip (bifoldr (flip (foldr f)) f)) z tu

instance (Traversable t, Traversable f) => Traversable (ScopeT t n f) where
  traverse f (ScopeT tu) = fmap ScopeT (traverse (bitraverse (traverse f) f) tu)

type instance BlankFunctor (ScopeT t n f) = f
type instance BlankInfo (ScopeT t n f) = n

instance (Adjunction t u, Applicative t) => BlankEmbed (ScopeT t n f) where
  blankEmbed = scopeTEmbed

instance (Adjunction t u, Monad t, Traversable t, Traversable f) => BlankAbstract (ScopeT t n f) where
  blankFree = scopeTFree
  blankAbstract = scopeTAbstract
  blankUnAbstract = scopeTUnAbstract
  blankInstantiate = scopeTInstantiate
  blankApply = scopeTApply

scopeTUnAdj :: (Applicative t, Adjunction t u) => u (ScopeT t n f a) -> ScopeT t n f a
scopeTUnAdj = counit . pure

scopeTWrapAdj :: Adjunction t u => UnderScope n f (ScopeT t n f a) a -> u (ScopeT t n f a)
scopeTWrapAdj = fmap ScopeT . unit

scopeTWrap :: Applicative t => UnderScope n f (ScopeT t n f a) a -> ScopeT t n f a
scopeTWrap = ScopeT . pure

scopeTBoundAdj :: Adjunction t u => Int -> u (ScopeT t n f a)
scopeTBoundAdj = scopeTWrapAdj . UnderBoundScope . BoundScope

scopeTBound :: Applicative t => Int -> ScopeT t n f a
scopeTBound = scopeTWrap . UnderBoundScope . BoundScope

scopeTFreeAdj :: Adjunction t u => a -> u (ScopeT t n f a)
scopeTFreeAdj = scopeTWrapAdj . UnderFreeScope . FreeScope

scopeTFree :: Applicative t => a -> ScopeT t n f a
scopeTFree = scopeTWrap . UnderFreeScope . FreeScope

scopeTBinderAdj :: Adjunction t u => Int -> n -> ScopeT t n f a -> u (ScopeT t n f a)
scopeTBinderAdj r n e = scopeTWrapAdj (UnderBinderScope (BinderScope r n e))

scopeTBinder :: Applicative t => Int -> n -> ScopeT t n f a -> ScopeT t n f a
scopeTBinder r n e = scopeTWrap (UnderBinderScope (BinderScope r n e))

scopeTEmbedAdj :: Adjunction t u => f (ScopeT t n f a) -> u (ScopeT t n f a)
scopeTEmbedAdj fe = fmap ScopeT (unit (UnderEmbedScope (EmbedScope fe)))

scopeTEmbed :: Applicative t => f (ScopeT t n f a) -> ScopeT t n f a
scopeTEmbed = scopeTWrap . UnderEmbedScope . EmbedScope

scopeTShift :: (Functor t, Functor f) => Int -> Int -> ScopeT t n f a -> ScopeT t n f a
scopeTShift c d (ScopeT tu) = ScopeT (fmap (underScopeShift scopeTShift c d) tu)

subScopeTBindOpt :: (Monad t, Functor f) => (a -> Maybe (t (UnderScope n f (ScopeT t n f a) a))) -> Int -> ScopeT t n f a -> ScopeT t n f a
subScopeTBindOpt g n (ScopeT tu) = ScopeT (tu >>= underScopeBindOpt scopeTShift subScopeTBindOpt g n)

scopeTBindOpt :: (Monad t, Functor f) => (a -> Maybe (ScopeT t n f a)) -> Int -> ScopeT t n f a -> ScopeT t n f a
scopeTBindOpt f = subScopeTBindOpt (fmap unScopeT . f)

subScopeTBind :: (Monad t, Functor f) => (a -> t (UnderScope n f (ScopeT t n f b) b)) -> Int -> ScopeT t n f a -> ScopeT t n f b
subScopeTBind g n (ScopeT tu) = ScopeT (tu >>= underScopeBind scopeTShift subScopeTBind g n)

scopeTBind :: (Monad t, Functor f) => (a -> ScopeT t n f b) -> Int -> ScopeT t n f a -> ScopeT t n f b
scopeTBind f = subScopeTBind (unScopeT . f)

scopeTCompact :: Monad t => t (ScopeT t n f a) -> ScopeT t n f a
scopeTCompact ts = ScopeT (ts >>= unScopeT)

subScopeTAbstract :: (Monad t, Functor f, Eq a) => Int -> n -> Seq a -> ScopeT t n f a -> ScopeT t n f a
subScopeTAbstract r n ks e =
  let f = fmap scopeTBound . flip Seq.elemIndexL ks
      e' = scopeTBindOpt f 0 e
  in scopeTBinder r n e'

scopeTAbstract :: (Monad t, Functor f, Eq a) => n -> Seq a -> ScopeT t n f a -> ScopeT t n f a
scopeTAbstract n ks =
  let r = Seq.length ks
  in subScopeTAbstract r n ks . scopeTShift 0 r

scopeTUnAbstract :: (Monad t, Traversable t, Functor f) => Seq a -> ScopeT t n f a -> ScopeT t n f a
scopeTUnAbstract ks = scopeTInstantiate (fmap scopeTFree ks)

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

scopeTLiftAnno :: Functor t => t a -> ScopeT t n f a
scopeTLiftAnno ta = ScopeT (fmap underScopePure ta)

scopeTHoistAnno :: (Functor t, Functor f) => (forall x. t x -> w x) -> ScopeT t n f a -> ScopeT w n f a
scopeTHoistAnno nat (ScopeT tu) = ScopeT (nat (fmap (first (scopeTHoistAnno nat)) tu))
