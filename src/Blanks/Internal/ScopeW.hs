{-# LANGUAGE UndecidableInstances #-}

-- | Internals.
module Blanks.Internal.ScopeW
  ( ScopeWC
  , ScopeW (..)
  , scopeWFree
  , scopeWAbstract
  , scopeWEmbed
  , scopeWBindFree
  , scopeWBindFree1
  , scopeWFillBound
  , scopeWFillBound1
  , scopeWUnBindFree
  , scopeWUnBindFree1
  , scopeWApply
  , scopeWApply1
  , scopeWBind
  , scopeWBindOpt
  , scopeWLift
  , scopeWLiftAnno
  , scopeWHoistAnno
  , scopeWMapAnno
  ) where

import Blanks.Internal.Abstract (Abstract (..), IsAbstractInfo (..), ShouldShift (..))
import Blanks.Internal.Under (UnderScope (..), underScopeShift)
import Blanks.Util.NatNewtype (NatNewtype, natNewtypeFrom, natNewtypeTo)
import Blanks.Util.Sub (SubError (..))
import Control.DeepSeq (NFData (..))
import Data.Bifoldable (bifoldr)
import Data.Bifunctor (bimap, first)
import Data.Bitraversable (bitraverse)
import Data.Functor.Adjunction (Adjunction (..))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
-- import Data.Sequence (Seq)
-- import qualified Data.Sequence as Seq

-- * ScopeW, patterns, and instances

-- | The core internal scope type. (The "w" comes from "wrapper".)
-- We wrap up an 'UnderScope' in some functor and demand that we
-- unwrap it in an adjoint context. In the first case, these functors will be
-- 'Identity', yielding the 'Scope' newtype. In the second case, these
-- functors will be 'Located' and 'Colocated', yielding the 'LocScope' newtype.
newtype ScopeW (t :: Type -> Type) (n :: Type -> Type) (f :: Type -> Type) (g :: Type -> Type) (a :: Type) = ScopeW
  { unScopeW :: t (UnderScope n f (g a) a)
  }

instance NFData (t (UnderScope n f (g a) a)) => NFData (ScopeW t n f g a) where
  rnf (ScopeW tu) = seq (rnf tu) ()

instance Eq (t (UnderScope n f (g a) a)) => Eq (ScopeW t n f g a) where
  ScopeW tu == ScopeW tv = tu == tv

instance Show (t (UnderScope n f (g a) a)) => Show (ScopeW t n f g a) where
  showsPrec d (ScopeW tu) = showString "ScopeW " . showParen True (showsPrec (d+1) tu)

instance (Functor t, Functor n, Functor f, Functor g) => Functor (ScopeW t n f g) where
  fmap f (ScopeW tu) = ScopeW (fmap (bimap (fmap f) f) tu)

instance (Foldable t, Foldable n, Foldable f, Foldable g) => Foldable (ScopeW t n f g) where
  foldr f z (ScopeW tu) = foldr (flip (bifoldr (flip (foldr f)) f)) z tu

instance (Traversable t, Traversable n, Traversable f, Traversable g) => Traversable (ScopeW t n f g) where
  traverse f (ScopeW tu) = fmap ScopeW (traverse (bitraverse (traverse f) f) tu)

type ScopeWC t u n f g = (Adjunction t u, Applicative u, IsAbstractInfo n, Functor f, NatNewtype (ScopeW t n f g) g)

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

scopeWAbstract :: ScopeWC t u n f g => Abstract n (g a) -> u (g a)
scopeWAbstract ab = fmap (natNewtypeTo . ScopeW) (unit (UnderScopeAbstract ab))

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
      UnderScopeAbstract ab ->
        let r = abstractInfoArity ab
        in scopeWAbstract (abstractInfoMapShouldShift (\_ ss -> let i' = case ss of { ShouldShiftYes -> i + r ; _ -> i } in scopeWBindN f i') ab)
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
      UnderScopeAbstract ab ->
        let r = abstractInfoArity ab
        in Just (scopeWAbstract (abstractInfoMapShouldShift (\_ ss -> let i' = case ss of { ShouldShiftYes -> i + r ; _ -> i } in scopeWBindOptN f i') ab))
      UnderScopeEmbed fe -> Just (scopeWEmbed (fmap (scopeWBindOptN f i) fe))

scopeWLift :: (ScopeWC t u n f g, Monad u, Traversable f) => f a -> u (g a)
scopeWLift fa = traverse scopeWFree fa >>= scopeWEmbed

-- * Abstraction and instantiation

scopeWBindFree :: (ScopeWC t u n f g, Eq a) => Seq a -> g a -> g a
scopeWBindFree ks e =
  let r = Seq.length ks
      e' = scopeWShift r e
      f = fmap scopeWBound . flip Seq.elemIndexL ks
  in scopeWBindOpt f e'

scopeWBindFree1 :: (ScopeWC t u n f g, Eq a) => a -> g a -> g a
scopeWBindFree1 = scopeWBindFree . Seq.singleton
{-# INLINE scopeWBindFree1 #-}

type FillC u n f =  (Functor u, IsAbstractInfo n, Functor f)

scopeWFillBound :: (ScopeWC t u n f g, FillC u n f) => Seq (u (g a)) -> g a -> g a
scopeWFillBound = scopeWFillBoundH 0
{-# INLINE scopeWFillBound #-}

scopeWFillBound1 :: (ScopeWC t u n f g, FillC u n f) => u (g a) -> g a -> g a
scopeWFillBound1 = scopeWFillBound . Seq.singleton
{-# INLINE scopeWFillBound1 #-}

scopeWFillBoundH :: (ScopeWC t u n f g, FillC u n f) => Int -> Seq (u (g a)) -> g a -> g a
scopeWFillBoundH h vs = scopeWModOpt (go h) where
  go i us =
    case us of
      UnderScopeBound b -> vs Seq.!? (b - i)
      UnderScopeFree _ -> Nothing
      UnderScopeAbstract ab@(Abstract info _) ->
        let r = abstractInfoArity info
            vs' = fmap (fmap (scopeWShift r)) vs
            -- sub in info with original or shifted vars
            ab' = abstractInfoMapShouldShift (\_ ss -> let (iX, vsX) = case ss of { ShouldShiftYes -> (i + r, vs') ; _ -> (i, vs) } in scopeWFillBoundH iX vsX) ab
        in Just (scopeWAbstract ab')
      UnderScopeEmbed fe -> Just (scopeWEmbed (fmap (scopeWFillBoundH i vs) fe))

scopeWUnBindFree :: (ScopeWC t u n f g, FillC u n f) => Seq a -> g a -> g a
scopeWUnBindFree = scopeWFillBound . fmap scopeWFree
{-# INLINE scopeWUnBindFree #-}

scopeWUnBindFree1 :: (ScopeWC t u n f g, FillC u n f) => a -> g a -> g a
scopeWUnBindFree1 = scopeWUnBindFree . Seq.singleton
{-# INLINE scopeWUnBindFree1 #-}

scopeWApply :: ScopeWC t u n f g => Seq (u (g a)) -> g a -> Either SubError (g a)
scopeWApply vs = scopeWModM go where
  go us =
    case us of
      UnderScopeAbstract ab ->
        let r = abstractInfoArity ab
            len = Seq.length vs
        in if len == r
              then Right (pure (scopeWShift (-1) (scopeWFillBound vs (abstractBody ab))))
              else Left (ApplyError len r)
      _ -> Left NonBinderError

scopeWApply1 :: ScopeWC t u n f g => u (g a) -> g a -> Either SubError (g a)
scopeWApply1 = scopeWApply . Seq.singleton
{-# INLINE scopeWApply1 #-}

-- * Annotation functions

scopeWLiftAnno :: (NatNewtype (ScopeW t n f g) g, Functor t) => t a -> g a
scopeWLiftAnno = natNewtypeTo . ScopeW . fmap UnderScopeFree

scopeWHoistAnno :: (NatNewtype (ScopeW t n f g) g, NatNewtype (ScopeW w n f h) h, Functor t, Functor w, Functor n, Functor f) => (forall x. t x -> w x) -> g a -> h a
scopeWHoistAnno nat ga =
  let ScopeW tu = natNewtypeFrom ga
      s = ScopeW (nat (fmap (first (scopeWHoistAnno nat)) tu))
  in natNewtypeTo s

scopeWMapAnno :: ScopeWC t u n f g => (t a -> t b) -> g a -> g b
scopeWMapAnno f = scopeWMod go where
  go us = case us of
    UnderScopeBound b -> scopeWBound b
    UnderScopeFree a -> fmap (natNewtypeTo . ScopeW . fmap UnderScopeFree . f) (unit a)
    UnderScopeAbstract ab -> scopeWAbstract (fmap (scopeWMapAnno f) ab)
    UnderScopeEmbed fe -> scopeWEmbed (fmap (scopeWMapAnno f) fe)
