{-# LANGUAGE UndecidableInstances #-}

module Blanks.Scope
  ( Scope (..)
  , pattern ScopeBound
  , pattern ScopeFree
  , pattern ScopeBinder
  , pattern ScopeEmbed
  , scopeBind
  , scopeBindOpt
  , scopeLift
  , scopeAbstract
  , scopeAbstract1
  , scopeUnAbstract
  , scopeUnAbstract1
  , scopeInstantiate
  , scopeInstantiate1
  , scopeApply
  , scopeApply1
  ) where

import Blanks.NatNewtype (NatNewtype)
import Blanks.ScopeW (ScopeW (ScopeW), scopeWAbstract, scopeWApply, scopeWBind, scopeWBindOpt, scopeWInstantiate,
                      scopeWLift, scopeWUnAbstract)
import Blanks.Sub (SubError)
import Blanks.UnderScope (pattern UnderScopeBinder, pattern UnderScopeBound, pattern UnderScopeEmbed,
                          pattern UnderScopeFree)
import Control.DeepSeq (NFData (..))
import Control.Monad (ap)
import Control.Monad.Identity (Identity (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- | A simple wrapper for your expression functor that knows how to name-bind.
-- See 'Blank' for usage, and see the patterns in this module for easy manipulation
-- and inspection.
newtype Scope n f a = Scope
  { unScope :: ScopeW Identity n f (Scope n f) a
  } deriving stock (Functor, Foldable, Traversable)

instance NatNewtype (ScopeW Identity n f (Scope n f)) (Scope n f)

instance (NFData n, NFData a, NFData (f (Scope n f a))) => NFData (Scope n f a) where
  rnf (Scope s) = seq (rnf s) ()

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
  pure = ScopeFree
  (<*>) = ap

instance Functor f => Monad (Scope n f) where
  return = pure
  s >>= f = scopeBind f s

instance (Eq (f (Scope n f a)), Eq n, Eq a) => Eq (Scope n f a) where
  Scope su == Scope sv = su == sv

instance (Show (f (Scope n f a)), Show n, Show a) => Show (Scope n f a) where
  showsPrec d (Scope (ScopeW tu)) = showString "Scope " . showsPrec (d+1) tu

-- * Interface

scopeBind :: Functor f => (a -> Scope n f b) -> Scope n f a -> Scope n f b
scopeBind f = scopeWBind (Identity . f)
{-# INLINE scopeBind #-}

scopeBindOpt :: Functor f => (a -> Maybe (Scope n f a)) -> Scope n f a -> Scope n f a
scopeBindOpt f = scopeWBindOpt (fmap Identity . f)
{-# INLINE scopeBindOpt #-}

scopeLift :: Traversable f => f a -> Scope n f a
scopeLift = runIdentity . scopeWLift
{-# INLINE scopeLift #-}

scopeAbstract :: (Functor f, Eq a) => n -> Seq a -> Scope n f a -> Scope n f a
scopeAbstract n ks = runIdentity . scopeWAbstract n ks
{-# INLINE scopeAbstract #-}

scopeAbstract1 :: (Functor f, Eq a) => n -> a -> Scope n f a -> Scope n f a
scopeAbstract1 n k = runIdentity . scopeWAbstract n (Seq.singleton k)
{-# INLINE scopeAbstract1 #-}

scopeUnAbstract :: Functor f => Seq a -> Scope n f a -> Scope n f a
scopeUnAbstract = scopeWUnAbstract
{-# INLINE scopeUnAbstract #-}

scopeUnAbstract1 :: Functor f => a -> Scope n f a -> Scope n f a
scopeUnAbstract1 = scopeWUnAbstract . Seq.singleton
{-# INLINE scopeUnAbstract1 #-}

scopeInstantiate :: Functor f => Seq (Scope n f a) -> Scope n f a -> Scope n f a
scopeInstantiate vs = scopeWInstantiate (fmap Identity vs)
{-# INLINE scopeInstantiate #-}

scopeInstantiate1 :: Functor f => Scope n f a -> Scope n f a -> Scope n f a
scopeInstantiate1 v = scopeWInstantiate (Seq.singleton (Identity v))
{-# INLINE scopeInstantiate1 #-}

scopeApply :: Functor f => Seq (Scope n f a) -> Scope n f a -> Either SubError (Scope n f a)
scopeApply vs = scopeWApply (fmap Identity vs)
{-# INLINE scopeApply #-}

scopeApply1 :: Functor f => Scope n f a -> Scope n f a -> Either SubError (Scope n f a)
scopeApply1 v = scopeWApply (Seq.singleton (Identity v))
{-# INLINE scopeApply1 #-}
