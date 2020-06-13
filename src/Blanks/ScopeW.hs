{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Blanks.ScopeW
  ( ScopeW (..)
  , ScopeWRawFold
  , ScopeWFold
  ) where

import Blanks.Class (Blank (..), BlankDomain, BlankEmbedded, BlankFold, BlankFunctor, BlankInfo, BlankRawFold)
import Blanks.Internal (BlankInternal (..), blankShift, defaultBlankBind, defaultBlankBindOpt, defaultBlankInstantiate)
import Blanks.NatTrans (RealNatIso)
import Blanks.RightAdjunct
import Blanks.Sub (SubError (..))
import Blanks.UnderScope (UnderScope, pattern UnderScopeBinder, pattern UnderScopeBound, pattern UnderScopeEmbed,
                          pattern UnderScopeFree, underScopeFold, underScopeShift)
import Data.Bifoldable (bifoldr)
import Data.Bifunctor (bimap)
import Data.Bitraversable (bitraverse)
import Data.Coerce (coerce)
import Data.Functor.Adjunction (Adjunction (..))
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- * ScopeW, patterns, and instances

newtype ScopeW t n f g a = ScopeW
  { unScopeW :: t (UnderScope n f (g a) a)
  }

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

type instance BlankDomain (ScopeW t n f g) = t
type instance BlankInfo (ScopeW t n f g) = n
type instance BlankFunctor (ScopeW t n f g) = f
type instance BlankEmbedded (ScopeW t n f g) = g

instance ScopeC t n f g => BlankInternal (ScopeW t n f g) where
  blankShiftN = scopeWShiftN
  blankBindN = scopeWBindN
  blankBindOptN = scopeWBindOptN
  blankInstantiateN = scopeWInstantiateN

instance ScopeC t n f g => Blank (ScopeW t n f g) where
  blankFree = scopeWFree
  blankAbstract = scopeWAbstract
  blankUnAbstract = scopeWUnAbstract
  blankInstantiate = defaultBlankInstantiate
  blankApply = scopeWApply
  blankBind = defaultBlankBind
  blankBindOpt = defaultBlankBindOpt
  blankEmbed = scopeWEmbed
  blankRawFold = scopeWRawFold
  blankFold = scopeWFold
  blankLiftAnno = scopeWLiftAnno

-- * Smart constructors, shift, and bind

type ScopeC t n f g =
  ( ApplicativeRightAdjunction t
  , Functor f
  , BlankDomain g ~ t
  -- , BlankCodomain g ~ RightAdjunct t
  , BlankInfo g ~ n
  , BlankFunctor g ~ f
  , BlankInternal g
  , RealNatIso (ScopeW t n f g) g
  )

scopeWMod :: RightAdjunction t => (UnderScope n f (g a) a -> RightAdjunct t x) -> ScopeW t n f g a -> x
scopeWMod f = rightAdjunct f . unScopeW

scopeWModOpt :: ApplicativeRightAdjunction t => (UnderScope n f (g a) a -> Maybe (RightAdjunct t (ScopeW t n f g a))) -> ScopeW t n f g a -> ScopeW t n f g a
scopeWModOpt f s = rightAdjunct (fromMaybe (pure s) . f) (unScopeW s)

scopeWModM :: (ApplicativeRightAdjunction t, Traversable m) => (UnderScope n f (g a) a -> m (RightAdjunct t x)) -> ScopeW t n f g a -> m x
scopeWModM f = rightAdjunct (sequenceA . f) . unScopeW

scopeWBound :: RightAdjunction t => Int -> RightAdjunct t (ScopeW t n f g a)
scopeWBound b = fmap ScopeW (unit (UnderScopeBound b))

scopeWFree :: RightAdjunction t => a -> RightAdjunct t (ScopeW t n f g a)
scopeWFree a = fmap ScopeW (unit (UnderScopeFree a))

scopeWShiftN :: ScopeC t n f g => Int -> Int -> ScopeW t n f g a -> ScopeW t n f g a
scopeWShiftN c d (ScopeW tu) = ScopeW (fmap (underScopeShift blankShiftN c d) tu)

scopeWBinder :: ScopeC t n f g => Int -> n -> g a -> RightAdjunct t (ScopeW t n f g a)
scopeWBinder r n e = fmap ScopeW (unit (UnderScopeBinder r n e))

scopeWEmbed :: RightAdjunction t => f (g a) -> RightAdjunct t (ScopeW t n f g a)
scopeWEmbed fe = fmap ScopeW (unit (UnderScopeEmbed fe))

relatedBindN :: ScopeC t n f g => (a -> RightAdjunct t (ScopeW t n f g b)) -> Int -> g a -> g b
relatedBindN f = blankBindN (fmap coerce . f)

scopeWBindN :: ScopeC t n f g => (a -> RightAdjunct t (ScopeW t n f g b)) -> Int -> ScopeW t n f g a -> ScopeW t n f g b
scopeWBindN f = scopeWMod . go where
  go i us =
    case us of
      UnderScopeBound b -> scopeWBound b
      UnderScopeFree a -> fmap (blankShift i) (f a)
      UnderScopeBinder r x e -> scopeWBinder r x (relatedBindN f (i + r) e)
      UnderScopeEmbed fe -> scopeWEmbed (fmap (relatedBindN f i) fe)

relatedBindOptN :: ScopeC t n f g => (a -> Maybe (RightAdjunct t (ScopeW t n f g a))) -> Int -> g a -> g a
relatedBindOptN f = blankBindOptN (fmap (fmap coerce) . f)

scopeWBindOptN :: ScopeC t n f g => (a -> Maybe (RightAdjunct t (ScopeW t n f g a))) -> Int -> ScopeW t n f g a -> ScopeW t n f g a
scopeWBindOptN f = scopeWModOpt . go where
  go i us =
    case us of
      UnderScopeBound _ -> Nothing
      UnderScopeFree a -> fmap (fmap (blankShift i)) (f a)
      UnderScopeBinder r x e -> Just (scopeWBinder r x (relatedBindOptN f (i + r) e))
      UnderScopeEmbed fe -> Just (scopeWEmbed (fmap (relatedBindOptN f i) fe))

-- * Abstraction

subScopeWAbstract :: (ScopeC t n f g, Eq a) => Int -> n -> Seq a -> ScopeW t n f g a -> RightAdjunct t (ScopeW t n f g a)
subScopeWAbstract r n ks e =
  let f = fmap scopeWBound . flip Seq.elemIndexL ks
      e' = blankBindOpt f e
      e'' = coerce e'
  in scopeWBinder r n e''

scopeWAbstract :: (ScopeC t n f g, Eq a) => n -> Seq a -> ScopeW t n f g a -> RightAdjunct t (ScopeW t n f g a)
scopeWAbstract n ks =
  let r = Seq.length ks
  in subScopeWAbstract r n ks . blankShift r

scopeWUnAbstract :: ScopeC t n f g => Seq a -> ScopeW t n f g a -> ScopeW t n f g a
scopeWUnAbstract ks = blankInstantiate (fmap scopeWFree ks)

scopeWInstantiateN :: ScopeC t n f g => Int -> Seq (RightAdjunct t (ScopeW t n f g a)) -> ScopeW t n f g a -> ScopeW t n f g a
scopeWInstantiateN h vs = scopeWModOpt (go h) where
  ws = fmap (fmap coerce) vs
  go i us =
    case us of
      UnderScopeBound b -> vs Seq.!? (b - i)
      UnderScopeFree _ -> Nothing
      UnderScopeBinder r n e ->
        let ws' = fmap (fmap (blankShift r)) ws
            e' = blankInstantiateN (r + i) ws' e
            e'' = coerce e'
        in Just (scopeWBinder r n e'')
      UnderScopeEmbed fe -> Just (scopeWEmbed (fmap (blankInstantiateN i ws) fe))

scopeWApply :: ScopeC t n f g => Seq (RightAdjunct t (ScopeW t n f g a)) -> ScopeW t n f g a -> Either SubError (ScopeW t n f g a)
scopeWApply vs = scopeWModM go where
  go us =
    case us of
      UnderScopeBinder r _ e ->
        let len = Seq.length vs
        in if len == r
              then Right (pure (blankShift (-1) (blankInstantiate vs (coerce e))))
              else Left (ApplyError len r)
      _ -> Left NonBinderError

-- * Folds

type ScopeWRawFold t n f g a r = BlankRawFold (ScopeW t n f g) a r
type ScopeWFold t n f g a r = BlankFold (ScopeW t n f g) a r

scopeWRawFold :: Functor t => ScopeWRawFold t n f g a r -> ScopeW t n f g a -> t r
scopeWRawFold usf = fmap (underScopeFold usf) . unScopeW

scopeWFold :: RightAdjunction t => ScopeWFold t n f g a r -> ScopeW t n f g a -> r
scopeWFold usf = counit . scopeWRawFold usf

-- * Annotation functions

scopeWLiftAnno :: Functor t => t a -> ScopeW t n f g a
scopeWLiftAnno ta = ScopeW (fmap UnderScopeFree ta)
