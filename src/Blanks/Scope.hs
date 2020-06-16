{-# LANGUAGE UndecidableInstances #-}

module Blanks.Scope
  ( Scope (..)
  , pattern ScopeBound
  , pattern ScopeFree
  , pattern ScopeBinder
  , pattern ScopeEmbed
  ) where

import Blanks.Interface (Blank, BlankFunctor, BlankInfo, BlankLeft, BlankRight, blankBind, blankFree)
import Blanks.NatNewtype (NatNewtype)
import Blanks.ScopeW (ScopeW (..))
import Blanks.UnderScope (pattern UnderScopeBinder, pattern UnderScopeBound, pattern UnderScopeEmbed,
                          pattern UnderScopeFree)
import Control.DeepSeq (NFData (..))
import Control.Monad (ap)
import Control.Monad.Identity (Identity (..))

-- | A simple wrapper for your expression functor that knows how to name-bind.
-- See 'Blank' for usage, and see the patterns in this module for easy manipulation
-- and inspection.
newtype Scope n f a = Scope
  { unScope :: ScopeW Identity n f (Scope n f) a
  } deriving stock (Functor, Foldable, Traversable)

type instance BlankLeft (Scope n f) = Identity
type instance BlankRight (Scope n f) = Identity
type instance BlankInfo (Scope n f) = n
type instance BlankFunctor (Scope n f) = f

instance Functor f => Blank (Scope n f)
instance NatNewtype (ScopeW Identity n f (Scope n f)) (Scope n f)

instance (NFData n, NFData a, NFData (f (Scope n f a))) => NFData (Scope n f a) where
  rnf = rnf . unScope

pattern ScopeBound :: Int -> Scope n f a
pattern ScopeBound b = Scope (ScopeW (Identity (UnderScopeBound b)))

pattern ScopeFree :: a -> Scope n f a
pattern ScopeFree a = Scope (ScopeW (Identity (UnderScopeFree a)))

pattern ScopeBinder :: Int -> n -> Scope n f a -> Scope n f a
pattern ScopeBinder i n e = Scope (ScopeW (Identity (UnderScopeBinder i n e)))

pattern ScopeEmbed :: f (Scope n f a) -> Scope n f a
pattern ScopeEmbed fe = Scope (ScopeW (Identity (UnderScopeEmbed fe)))

{-# COMPLETE ScopeBound, ScopeFree, ScopeBinder, ScopeEmbed #-}

instance Functor f => Applicative (Scope n f) where
  pure = runIdentity . blankFree
  (<*>) = ap

instance Functor f => Monad (Scope n f) where
  return = pure
  s >>= f = blankBind (Identity . f) s

instance (Eq (f (Scope n f a)), Eq n, Eq a) => Eq (Scope n f a) where
  Scope su == Scope sv = su == sv

instance (Show (f (Scope n f a)), Show n, Show a) => Show (Scope n f a) where
  showsPrec d (Scope (ScopeW tu)) = showString "Scope " . showsPrec (d+1) tu
