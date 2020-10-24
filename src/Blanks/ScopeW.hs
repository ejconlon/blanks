{-# LANGUAGE UndecidableInstances #-}

-- | Internals.
module Blanks.ScopeW
  ( ScopeWC
  , ScopeW (..)
  , scopeWFree
  , scopeWEmbed
  , scopeWFromInnerBinder
  , scopeWInnerBinder
  , scopeWInnerBinder1
  , scopeWAbstract
  , scopeWAbstract1
  , scopeWUnAbstract
  , scopeWUnAbstract1
  , scopeWInstantiate
  , scopeWInstantiate1
  , scopeWApply
  , scopeWApply1
  , scopeWBind
  , scopeWBindOpt
  , scopeWLift
  , scopeWLiftAnno
  , scopeWHoistAnno
  , scopeWMapAnno
  ) where

import Blanks.Core (BinderScope (..))
import Blanks.NatNewtype (NatNewtype, natNewtypeFrom, natNewtypeTo)
import Blanks.Sub (SubError (..))
import Blanks.Under (UnderScope (..), pattern UnderScopeBinder, pattern UnderScopeBound, pattern UnderScopeEmbed,
                     pattern UnderScopeFree, underScopeShift)
import Control.DeepSeq (NFData (..))
import Data.Bifoldable (bifoldr)
import Data.Bifunctor (bimap, first)
import Data.Bitraversable (bitraverse)
import Data.Functor.Adjunction (Adjunction (..))
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- * ScopeW, patterns, and instances

-- | The core internal scope type. (The "w" comes from "wrapper".)
-- We wrap up an 'UnderScope' in some functor and demand that we
-- unwrap it in an adjoint context. In the first case, these functors will be
-- 'Identity', yielding the 'Scope' newtype. In the second case, these
-- functors will be 'Located' and 'Colocated', yielding the 'LocScope' newtype.
newtype ScopeW t n f g a = ScopeW
  { unScopeW :: t (UnderScope n f (g a) a)
  }

instance NFData (t (UnderScope n f (g a) a)) => NFData (ScopeW t n f g a) where
  rnf (ScopeW tu) = seq (rnf tu) ()

instance Eq (t (UnderScope n f (g a) a)) => Eq (ScopeW t n f g a) where
  ScopeW tu == ScopeW tv = tu == tv

instance Show (t (UnderScope n f (g a) a)) => Show (ScopeW t n f g a) where
  showsPrec d (ScopeW tu) = showString "ScopeW " . showsPrec (d+1) tu

instance (Functor t, Functor f, Functor g) => Functor (ScopeW t n f g) where
  fmap f (ScopeW tu) = ScopeW (fmap (bimap (fmap f) f) tu)

instance (Foldable t, Foldable f, Foldable g) => Foldable (ScopeW t n f g) where
  foldr f z (ScopeW tu) = foldr (flip (bifoldr (flip (foldr f)) f)) z tu

instance (Traversable t, Traversable f, Traversable g) => Traversable (ScopeW t n f g) where
  traverse f (ScopeW tu) = fmap ScopeW (traverse (bitraverse (traverse f) f) tu)

type ScopeWC t u n f g = (Adjunction t u, Applicative u, Functor f, NatNewtype (ScopeW t n f g) g)

-- * Smart constructors, shift, and bind

scopeWMod :: ScopeWC t u n f g => (UnderScope n f (g a) a -> u x) -> g a -> x
scopeWMod f = rightAdjunct f . unScopeW . natNewtypeFrom
{-# INLINE scopeWMod #-}

scopeWModOpt :: ScopeWC t u n f g => (UnderScope n f (g a) a -> Maybe (u (g a))) -> g a -> g a
scopeWModOpt f s = rightAdjunct (fromMaybe (pure s) . f) (unScopeW (natNewtypeFrom s))
{-# INLINE scopeWModOpt #-}

scopeWModM :: (ScopeWC t u n f g, Traversable m) => (UnderScope n f (g a) a -> m (u x)) -> g a -> m x
scopeWModM f = rightAdjunct (sequenceA . f) . unScopeW . natNewtypeFrom
{-# INLINE scopeWModM #-}

scopeWBound :: ScopeWC t u n f g => Int -> u (g a)
scopeWBound b = fmap (natNewtypeTo . ScopeW) (unit (UnderScopeBound b))

scopeWFree :: ScopeWC t u n f g => a -> u (g a)
scopeWFree a = fmap (natNewtypeTo . ScopeW) (unit (UnderScopeFree a))

scopeWShift :: ScopeWC t u n f g => Int -> g a -> g a
scopeWShift = scopeWShiftN 0
{-# INLINE scopeWShift #-}

scopeWShiftN :: ScopeWC t u n f g => Int -> Int -> g a -> g a
scopeWShiftN c d e =
  let ScopeW tu = natNewtypeFrom e
  in natNewtypeTo (ScopeW (fmap (underScopeShift scopeWShiftN c d) tu))

scopeWBinder :: ScopeWC t u n f g => Int -> n -> g a -> u (g a)
scopeWBinder r n e = fmap (natNewtypeTo . ScopeW) (unit (UnderScopeBinder r n e))

scopeWFromInnerBinder :: ScopeWC t u n f g => BinderScope n (g a) -> u (g a)
scopeWFromInnerBinder b = fmap (natNewtypeTo . ScopeW) (unit (UnderBinderScope b))

scopeWEmbed :: ScopeWC t u n f g => f (g a) -> u (g a)
scopeWEmbed fe = fmap (natNewtypeTo . ScopeW) (unit (UnderScopeEmbed fe))

scopeWBind :: ScopeWC t u n f g => (a -> u (g b)) -> g a -> g b
scopeWBind f = scopeWBindN f 0
{-# INLINE scopeWBind #-}

scopeWBindN :: ScopeWC t u n f g => (a -> u (g b)) -> Int -> g a -> g b
scopeWBindN f = scopeWMod . go where
  go i us =
    case us of
      UnderScopeBound b -> scopeWBound b
      UnderScopeFree a -> fmap (scopeWShift i) (f a)
      UnderScopeBinder r x e -> scopeWBinder r x (scopeWBindN f (i + r) e)
      UnderScopeEmbed fe -> scopeWEmbed (fmap (scopeWBindN f i) fe)

scopeWBindOpt :: ScopeWC t u n f g => (a -> Maybe (u (g a))) -> g a -> g a
scopeWBindOpt f = scopeWBindOptN f 0
{-# INLINE scopeWBindOpt #-}

scopeWBindOptN :: ScopeWC t u n f g => (a -> Maybe (u (g a))) -> Int -> g a -> g a
scopeWBindOptN f = scopeWModOpt . go where
  go i us =
    case us of
      UnderScopeBound _ -> Nothing
      UnderScopeFree a -> fmap (fmap (scopeWShift i)) (f a)
      UnderScopeBinder r x e -> Just (scopeWBinder r x (scopeWBindOptN f (i + r) e))
      UnderScopeEmbed fe -> Just (scopeWEmbed (fmap (scopeWBindOptN f i) fe))

scopeWLift :: (ScopeWC t u n f g, Monad u, Traversable f) => f a -> u (g a)
scopeWLift fa = traverse scopeWFree fa >>= scopeWEmbed

-- * Abstraction

scopeWInnerBinder :: (ScopeWC t u n f g, Eq a) => n -> Seq a -> g a -> BinderScope n (g a)
scopeWInnerBinder n ks e =
  let r = Seq.length ks
      e' = scopeWShift r e
      f = fmap scopeWBound . flip Seq.elemIndexL ks
      e'' = scopeWBindOpt f e'
  in BinderScope r n e''

scopeWInnerBinder1 :: (ScopeWC t u n f g, Eq a) => n -> a -> g a -> BinderScope n (g a)
scopeWInnerBinder1 n = scopeWInnerBinder n . Seq.singleton
{-# INLINE scopeWInnerBinder1 #-}

scopeWAbstract :: (ScopeWC t u n f g, Eq a) => n -> Seq a -> g a -> u (g a)
scopeWAbstract n ks e = scopeWFromInnerBinder (scopeWInnerBinder n ks e)
{-# INLINE scopeWAbstract #-}

scopeWAbstract1 :: (ScopeWC t u n f g, Eq a) => n -> a -> g a -> u (g a)
scopeWAbstract1 n = scopeWAbstract n . Seq.singleton
{-# INLINE scopeWAbstract1 #-}

scopeWUnAbstract :: ScopeWC t u n f g => Seq a -> g a -> g a
scopeWUnAbstract ks = scopeWInstantiate (fmap scopeWFree ks)
{-# INLINE scopeWUnAbstract #-}

scopeWUnAbstract1 :: ScopeWC t u n f g => a -> g a -> g a
scopeWUnAbstract1 = scopeWUnAbstract . Seq.singleton
{-# INLINE scopeWUnAbstract1 #-}

scopeWInstantiate :: ScopeWC t u n f g => Seq (u (g a)) -> g a -> g a
scopeWInstantiate = scopeWInstantiateN 0
{-# INLINE scopeWInstantiate #-}

scopeWInstantiate1 :: ScopeWC t u n f g => u (g a) -> g a -> g a
scopeWInstantiate1 = scopeWInstantiate . Seq.singleton
{-# INLINE scopeWInstantiate1 #-}

scopeWInstantiateN :: ScopeWC t u n f g => Int -> Seq (u (g a)) -> g a -> g a
scopeWInstantiateN h vs = scopeWModOpt (go h) where
  go i us =
    case us of
      UnderScopeBound b -> vs Seq.!? (b - i)
      UnderScopeFree _ -> Nothing
      UnderScopeBinder r n e ->
        let vs' = fmap (fmap (scopeWShift r)) vs
            e' = scopeWInstantiateN (r + i) vs' e
        in Just (scopeWBinder r n e')
      UnderScopeEmbed fe -> Just (scopeWEmbed (fmap (scopeWInstantiateN i vs) fe))

scopeWApply :: ScopeWC t u n f g => Seq (u (g a)) -> g a -> Either SubError (g a)
scopeWApply vs = scopeWModM go where
  go us =
    case us of
      UnderScopeBinder r _ e ->
        let len = Seq.length vs
        in if len == r
              then Right (pure (scopeWShift (-1) (scopeWInstantiate vs e)))
              else Left (ApplyError len r)
      _ -> Left NonBinderError

scopeWApply1 :: ScopeWC t u n f g => u (g a) -> g a -> Either SubError (g a)
scopeWApply1 = scopeWApply . Seq.singleton
{-# INLINE scopeWApply1 #-}

-- * Annotation functions

scopeWLiftAnno :: (NatNewtype (ScopeW t n f g) g, Functor t) => t a -> g a
scopeWLiftAnno = natNewtypeTo . ScopeW . fmap UnderScopeFree

scopeWHoistAnno :: (NatNewtype (ScopeW t n f g) g, NatNewtype (ScopeW w n f h) h, Functor t, Functor w, Functor f) => (forall x. t x -> w x) -> g a -> h a
scopeWHoistAnno nat ga =
  let ScopeW tu = natNewtypeFrom ga
      s = ScopeW (nat (fmap (first (scopeWHoistAnno nat)) tu))
  in natNewtypeTo s

scopeWMapAnno :: ScopeWC t u n f g => (t a -> t b) -> g a -> g b
scopeWMapAnno f = scopeWMod go where
  go us = case us of
    UnderScopeBound b -> scopeWBound b
    UnderScopeFree a -> fmap (natNewtypeTo . ScopeW . fmap UnderScopeFree . f) (unit a)
    UnderScopeBinder r x e -> scopeWBinder r x (scopeWMapAnno f e)
    UnderScopeEmbed fe -> scopeWEmbed (fmap (scopeWMapAnno f) fe)
