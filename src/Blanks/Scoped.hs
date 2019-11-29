{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Blanks.Scoped
  ( Scoped (..)
  , ScopedFold
  , ScopedType
  , boundScoped
  , reviewBoundScoped
  , freeScoped
  , reviewFreeScoped
  , binderScoped
  , reviewBinderScoped
  , embedScoped
  , wrapScoped
  , liftScoped
  , abstractScoped
  , abstract1Scoped
  , unAbstractScoped
  , unAbstract1Scoped
  , instantiateScoped
  , instantiate1Scoped
  , foldScoped
  ) where

import Blanks.Fold
import Blanks.Scope
import Control.Lens (Iso', Prism', from, over, prism, review, simple, view, withPrism)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

class Scoped h where
  type ScopedInfo h :: *
  type ScopedFunctor h :: * -> *
  type ScopedIdentifier h :: *

  scoped :: Iso' h (ScopedType h)

type ScopedType h = Scope (ScopedInfo h) (ScopedFunctor h) (ScopedIdentifier h)

type ScopedFold h r = ScopeFold (ScopedInfo h) (ScopedFunctor h) (ScopedIdentifier h) r

boundScoped :: Scoped h => Prism' h BoundScope
boundScoped = scoped . _UnderScope . _UnderBoundScope

reviewBoundScoped :: Scoped h => Int -> h
reviewBoundScoped = review boundScoped . BoundScope

freeScoped :: Scoped h => Prism' h (FreeScope (ScopedIdentifier h))
freeScoped = scoped . _UnderScope . _UnderFreeScope

reviewFreeScoped :: Scoped h => ScopedIdentifier h -> h
reviewFreeScoped = review freeScoped . FreeScope

binderScoped' :: Scoped h => Prism' h (BinderScope (ScopedInfo h) (ScopedType h))
binderScoped' = scoped . _UnderScope . _UnderBinderScope

-- Jesus...
underMap :: Functor f => Prism' a (f b) -> Iso' b c -> Prism' a (f c)
underMap v w =
  withPrism v $ \m n ->
    let p fc =
          let fb = fmap (review w) fc
           in m fb
        q a =
          let b = n a
           in fmap (fmap (view w)) b
     in prism p q

binderScoped :: Scoped h => Prism' h (BinderScope (ScopedInfo h) h)
binderScoped = underMap binderScoped' (from scoped)

reviewBinderScoped :: Scoped h => BinderScope (ScopedInfo h) h -> h
reviewBinderScoped = review binderScoped

embedScoped' :: Scoped h => Prism' h (EmbedScope (ScopedFunctor h) (ScopedType h))
embedScoped' = scoped . _UnderScope . _UnderEmbedScope

embedScoped :: (Scoped h, Functor (ScopedFunctor h)) => Prism' h (EmbedScope (ScopedFunctor h) h)
embedScoped = underMap embedScoped' (from scoped)

wrapScoped' :: Scoped h => ScopedFunctor h (ScopedType h) -> h
wrapScoped' = review scoped . Scope . UnderEmbedScope . EmbedScope

wrapScoped :: (Scoped h, Functor (ScopedFunctor h)) => ScopedFunctor h h -> h
wrapScoped = wrapScoped' . fmap (view scoped)

liftScoped :: (Scoped h, Functor (ScopedFunctor h)) => ScopedFunctor h (ScopedIdentifier h) -> h
liftScoped fa =
  let fs = Scope . UnderFreeScope . FreeScope <$> fa
   in wrapScoped' fs

abstractScoped ::
     (Scoped h, Functor (ScopedFunctor h), Eq (ScopedIdentifier h))
  => ScopedInfo h
  -> Seq (ScopedIdentifier h)
  -> h
  -> h
abstractScoped si sas = over scoped (Scope . UnderBinderScope . abstract si sas)

abstract1Scoped ::
     (Scoped h, Functor (ScopedFunctor h), Eq (ScopedIdentifier h)) => ScopedInfo h -> ScopedIdentifier h -> h -> h
abstract1Scoped si sa = abstractScoped si (Seq.singleton sa)

unAbstractScoped :: (Scoped h, Functor (ScopedFunctor h)) => Seq (ScopedIdentifier h) -> h -> h
unAbstractScoped sas = over scoped (unAbstract sas)

unAbstract1Scoped :: (Scoped h, Functor (ScopedFunctor h)) => ScopedIdentifier h -> h -> h
unAbstract1Scoped sa = over scoped (unAbstract1 sa)

instantiateScoped :: (Scoped h, Functor (ScopedFunctor h)) => Seq h -> h -> h
instantiateScoped vs = over scoped (instantiate (fmap (review (from scoped)) vs))

instantiate1Scoped :: (Scoped h, Functor (ScopedFunctor h)) => h -> h -> h
instantiate1Scoped v = instantiateScoped (Seq.singleton v)

foldScoped :: Scoped h => ScopedFold h r -> h -> r
foldScoped f = foldScope f . view scoped

-- shiftScoped :: (Scoped h, Functor (ScopedFunctor h)) => Int -> h -> h
-- shiftScoped i = over scoped (scopeShift i)

-- TODO(ejconlon) Finish these
-- applyScoped :: (ThrowSub m, Applicative m, Scoped h, Functor (ScopedFunctor h)) => Seq h -> h -> m h
-- applyScoped vs = undefined -- over scoped (apply (fmap (review (from scoped)) vs))
-- apply1Scoped :: (ThrowSub m, Applicative m, Scoped h, Functor (ScopedFunctor h)) => h -> h -> m h
-- apply1Scoped v = applyScoped (Seq.singleton v)

instance Scoped (Scope n f a) where
  type ScopedInfo (Scope n f a) = n
  type ScopedFunctor (Scope n f a) = f
  type ScopedIdentifier (Scope n f a) = a

  scoped = simple
