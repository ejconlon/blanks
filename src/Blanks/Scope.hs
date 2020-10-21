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
import Blanks.ScopeW (ScopeW (ScopeW), scopeWAbstract, scopeWAbstract1, scopeWApply, scopeWApply1, scopeWBind,
                      scopeWBindOpt, scopeWInstantiate, scopeWInstantiate1, scopeWLift, scopeWUnAbstract,
                      scopeWUnAbstract1)
import Blanks.Sub (SubError)
import Blanks.Under (pattern UnderScopeBinder, pattern UnderScopeBound, pattern UnderScopeEmbed,
                     pattern UnderScopeFree)
import Control.DeepSeq (NFData (..))
import Control.Monad (ap)
import Control.Monad.Identity (Identity (..))
import Data.Sequence (Seq)

-- | A simple wrapper for your expression functor that knows how to name-bind.
-- Create free variables is 'pure', bind them with '>>=', and list free variables with folds.
-- See 'LocScope' for a version with additional annotations between layers.
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

-- | Substitution as a kind of monadic bind.
scopeBind :: Functor f => (a -> Scope n f b) -> Scope n f a -> Scope n f b
scopeBind f = scopeWBind (Identity . f)
{-# INLINE scopeBind #-}

-- | Optional substitution as another kind of monadic bind.
scopeBindOpt :: Functor f => (a -> Maybe (Scope n f a)) -> Scope n f a -> Scope n f a
scopeBindOpt f = scopeWBindOpt (fmap Identity . f)
{-# INLINE scopeBindOpt #-}

-- | Lift an expression functor into the scope functor.
scopeLift :: Traversable f => f a -> Scope n f a
scopeLift = runIdentity . scopeWLift
{-# INLINE scopeLift #-}

-- | Binds free variables in an expression and returns a binder.
scopeAbstract
  :: (Functor f, Eq a)
  => n
  -- ^ Annotation specific to your expression functor.
  -- Might contain original variable names and types, or might
  -- mark this as a "let" vs a "lambda"
  -> Seq a
  -- ^ Free variables to bind, like the names of function parameters
  -> Scope n f a
  -- ^ The expression to bind in, like the body of a function
  -> Scope n f a
scopeAbstract n ks = runIdentity . scopeWAbstract n ks
{-# INLINE scopeAbstract #-}

-- | 'scopeAbstract' for a single argument.
scopeAbstract1 :: (Functor f, Eq a) => n -> a -> Scope n f a -> Scope n f a
scopeAbstract1 n k = runIdentity . scopeWAbstract1 n k
{-# INLINE scopeAbstract1 #-}

-- | Un-bind free variables in an expression. Basically the inverse of
-- 'scopeAbstract'. Take care to match the arity of the binder! ('scopeApply' is safer.)
scopeUnAbstract
  :: Functor f
  => Seq a
  -- ^ The names of the variables you're freeing
  -> Scope n f a
   -- ^ The expression to substitute in (typically a binder)
  -> Scope n f a
scopeUnAbstract = scopeWUnAbstract
{-# INLINE scopeUnAbstract #-}

-- 'scopeUnAbstract' for a single argument.
scopeUnAbstract1 :: Functor f => a -> Scope n f a -> Scope n f a
scopeUnAbstract1 = scopeWUnAbstract1
{-# INLINE scopeUnAbstract1 #-}

-- | Instantiate the bound variables in an expression with other expressions.
-- Take care to match the arity of the binder! ('scopeApply' is safer.)
scopeInstantiate
  :: Functor f
  => Seq (Scope n f a)
  -- ^ Expressions to substitute in place of bound vars
  -> Scope n f a
  -- ^ The expression to substitute in (typically a binder)
  -> Scope n f a
scopeInstantiate vs = scopeWInstantiate (fmap Identity vs)
{-# INLINE scopeInstantiate #-}

-- | 'scopeInstantiate' for a single argument.
scopeInstantiate1 :: Functor f => Scope n f a -> Scope n f a -> Scope n f a
scopeInstantiate1 v = scopeWInstantiate1 (Identity v)
{-# INLINE scopeInstantiate1 #-}

-- | Instantiates the bound variables in an expression with other expressions.
-- Throws errors on mismatched arity, non binder expression, unbound vars, etc.
-- A version of 'scopeInstantiate' that fails loudly instead of silently!
scopeApply
  :: Functor f
  => Seq (Scope n f a)
  -- ^ Expressions to substitute in place of bound vars
  -> Scope n f a
  -- ^ The binder expression to substitute in
  -> Either SubError (Scope n f a)
scopeApply vs = scopeWApply (fmap Identity vs)
{-# INLINE scopeApply #-}

-- | 'scopeApply' for a single argument.
scopeApply1 :: Functor f => Scope n f a -> Scope n f a -> Either SubError (Scope n f a)
scopeApply1 v = scopeWApply1 (Identity v)
{-# INLINE scopeApply1 #-}
