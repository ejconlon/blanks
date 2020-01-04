{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Blanks.ScopeT
  ( ScopeT (..)
  , ScopeTFold
  , scopeTBind
  , scopeTEmbed
  , scopeTFold
  , scopeTFree
  , scopeTHoistAnno
  , scopeTLiftAnno
  , scopeTRawFold
  ) where

import Blanks.Class
import Blanks.RightAdjunct
import Blanks.Sub (SubError (..))
import Blanks.UnderScope (BinderScope (..), BoundScope (..), EmbedScope (..), FreeScope (..), UnderScope (..),
                          UnderScopeFold (..), underScopeFold, underScopePure, underScopeShift)
import Data.Bifoldable (bifoldr)
import Data.Bifunctor (bimap, first)
import Data.Bitraversable (bitraverse)
import Data.Functor.Adjunction (Adjunction (..))
import Data.Maybe (fromMaybe)
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
type instance BlankCodomain (ScopeT t n f) = RightAdjunct t

instance RightAdjunction t => BlankEmbed (ScopeT t n f) where
  blankEmbed = scopeTEmbed

instance (RightAdjunctionApplicative t, Functor f) => BlankAbstract (ScopeT t n f) where
  blankFree = scopeTFree
  blankAbstract = scopeTAbstract
  blankUnAbstract = scopeTUnAbstract
  blankInstantiate = scopeTInstantiate
  blankApply = scopeTApply

scopeTWrap :: RightAdjunction t => UnderScope n f (ScopeT t n f a) a -> RightAdjunct t (ScopeT t n f a)
scopeTWrap = fmap ScopeT . unit

scopeTBound :: RightAdjunction t => Int -> RightAdjunct t (ScopeT t n f a)
scopeTBound = scopeTWrap . UnderBoundScope . BoundScope

scopeTFree :: RightAdjunction t => a -> RightAdjunct t (ScopeT t n f a)
scopeTFree = scopeTWrap . UnderFreeScope . FreeScope

scopeTBinder :: RightAdjunction t => Int -> n -> ScopeT t n f a -> RightAdjunct t (ScopeT t n f a)
scopeTBinder r n e = scopeTWrap (UnderBinderScope (BinderScope r n e))

scopeTEmbed :: RightAdjunction t => f (ScopeT t n f a) -> RightAdjunct t (ScopeT t n f a)
scopeTEmbed fe = fmap ScopeT (unit (UnderEmbedScope (EmbedScope fe)))

scopeTShiftN :: (Functor t, Functor f) => Int -> Int -> ScopeT t n f a -> ScopeT t n f a
scopeTShiftN c d (ScopeT tu) = ScopeT (fmap (underScopeShift scopeTShiftN c d) tu)

scopeTShift :: (Functor t, Functor f) => Int -> ScopeT t n f a -> ScopeT t n f a
scopeTShift = scopeTShiftN 0

scopeTBindOptN :: (RightAdjunctionApplicative t, Functor f) => (a -> Maybe (RightAdjunct t (ScopeT t n f a))) -> Int -> ScopeT t n f a -> ScopeT t n f a
scopeTBindOptN f = scopeTModOpt . go where
  go i us =
    case us of
      UnderBoundScope _ -> Nothing
      UnderFreeScope (FreeScope a) -> fmap (fmap (scopeTShift i)) (f a)
      UnderBinderScope (BinderScope r x e) -> Just (scopeTBinder r x (scopeTBindOptN f (i + r) e))
      UnderEmbedScope (EmbedScope fe) -> Just (scopeTEmbed (fmap (scopeTBindOptN f i) fe))

scopeTBindOpt :: (RightAdjunctionApplicative t, Functor f) => (a -> Maybe (RightAdjunct t (ScopeT t n f a))) -> ScopeT t n f a -> ScopeT t n f a
scopeTBindOpt f = scopeTBindOptN f 0

scopeTBindN :: (RightAdjunction t, Functor f) => (a -> RightAdjunct t (ScopeT t n f b)) -> Int -> ScopeT t n f a -> ScopeT t n f b
scopeTBindN f = scopeTMod . go where
  go i us =
    case us of
      UnderBoundScope (BoundScope b) -> scopeTBound b
      UnderFreeScope (FreeScope a) -> fmap (scopeTShift i) (f a)
      UnderBinderScope (BinderScope r x e) -> scopeTBinder r x (scopeTBindN f (i + r) e)
      UnderEmbedScope (EmbedScope fe) -> scopeTEmbed (fmap (scopeTBindN f i) fe)

scopeTBind :: (RightAdjunction t, Functor f) => (a -> RightAdjunct t (ScopeT t n f b)) -> ScopeT t n f a -> ScopeT t n f b
scopeTBind f = scopeTBindN f 0

subScopeTAbstract :: (RightAdjunctionApplicative t, Functor f, Eq a) => Int -> n -> Seq a -> ScopeT t n f a -> RightAdjunct t (ScopeT t n f a)
subScopeTAbstract r n ks e =
  let f = fmap scopeTBound . flip Seq.elemIndexL ks
      e' = scopeTBindOpt f e
  in scopeTBinder r n e'

scopeTAbstract :: (RightAdjunctionApplicative t, Functor f, Eq a) => n -> Seq a -> ScopeT t n f a -> RightAdjunct t (ScopeT t n f a)
scopeTAbstract n ks =
  let r = Seq.length ks
  in subScopeTAbstract r n ks . scopeTShift r

scopeTUnAbstract :: (RightAdjunctionApplicative t, Functor f) => Seq a -> ScopeT t n f a -> ScopeT t n f a
scopeTUnAbstract ks = scopeTInstantiate (fmap scopeTFree ks)

scopeTModOpt :: RightAdjunctionApplicative t => (UnderScope n f (ScopeT t n f a) a -> Maybe (RightAdjunct t (ScopeT t n f a))) -> ScopeT t n f a -> ScopeT t n f a
scopeTModOpt f s = rightAdjunct (fromMaybe (pure s) . f) (unScopeT s)

scopeTModM :: (RightAdjunctionApplicative t, Traversable m) => (UnderScope n f (ScopeT t n f a) a -> m (RightAdjunct t x)) -> ScopeT t n f a -> m x
scopeTModM f = rightAdjunct (sequenceA . f) . unScopeT

scopeTMod :: RightAdjunction t => (UnderScope n f (ScopeT t n f a) a -> RightAdjunct t x) -> ScopeT t n f a -> x
scopeTMod f = rightAdjunct f . unScopeT

scopeTInstantiateN :: (RightAdjunctionApplicative t, Functor f) => Int -> Seq (RightAdjunct t (ScopeT t n f a)) -> ScopeT t n f a -> ScopeT t n f a
scopeTInstantiateN h vs = scopeTModOpt (go h vs) where
  go i ws us =
    case us of
      UnderBoundScope (BoundScope b) -> vs Seq.!? (b - i)
      UnderFreeScope _ -> Nothing
      UnderBinderScope (BinderScope r n e) ->
        let ws' = fmap (fmap (scopeTShift r)) ws
            e' = scopeTInstantiateN (r + i) ws' e
        in Just (scopeTBinder r n e')
      UnderEmbedScope (EmbedScope fe) -> Just (scopeTEmbed (fmap (scopeTInstantiateN i ws) fe))

scopeTInstantiate :: (RightAdjunctionApplicative t,  Functor f) => Seq (RightAdjunct t (ScopeT t n f a)) -> ScopeT t n f a -> ScopeT t n f a
scopeTInstantiate = scopeTInstantiateN 0

scopeTApply :: (RightAdjunctionApplicative t, Functor f) => Seq (RightAdjunct t (ScopeT t n f a)) -> ScopeT t n f a -> Either SubError (ScopeT t n f a)
scopeTApply vs = scopeTModM go where
  go us =
    case us of
      UnderBinderScope (BinderScope r _ e) ->
        let len = Seq.length vs
        in if len == r
              then Right (pure (scopeTShift (-1) (scopeTInstantiate vs e)))
              else Left (ApplyError len r)
      _ -> Left NonBinderError

type ScopeTFold t n f a r = UnderScopeFold n f (ScopeT t n f a) a (RightAdjunct t r)

scopeTRawFold :: Functor t => UnderScopeFold n f (ScopeT t n f a) a r -> ScopeT t n f a -> t r
scopeTRawFold usf = fmap (underScopeFold usf) . unScopeT

scopeTFold :: RightAdjunction t => ScopeTFold t n f a r -> ScopeT t n f a -> r
scopeTFold usf = counit . scopeTRawFold usf

scopeTLiftAnno :: Functor t => t a -> ScopeT t n f a
scopeTLiftAnno ta = ScopeT (fmap underScopePure ta)

scopeTHoistAnno :: (Functor t, Functor f) => (forall x. t x -> w x) -> ScopeT t n f a -> ScopeT w n f a
scopeTHoistAnno nat (ScopeT tu) = ScopeT (nat (fmap (first (scopeTHoistAnno nat)) tu))
