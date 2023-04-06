{-# LANGUAGE UndecidableInstances #-}

module Blanks.Scope
  ( Scope (..)
  , pattern ScopeBound
  , pattern ScopeFree
  , pattern ScopeAbstract
  , pattern ScopeEmbed
  , scopeBind
  , scopeBindOpt
  , scopeLift
  , scopeBindFree
  , scopeBindFree1
  , scopeFillBound
  , scopeFillBound1
  , scopeUnBindFree
  , scopeUnBindFree1
  , scopeApply
  , scopeApply1
  )
where

import Blanks.Internal.Abstract (Abstract, IsAbstractInfo)
import Blanks.Internal.ScopeW
  ( ScopeW (..)
  , scopeWApply
  , scopeWApply1
  , scopeWBind
  , scopeWBindFree
  , scopeWBindFree1
  , scopeWBindOpt
  , scopeWFillBound
  , scopeWFillBound1
  , scopeWLift
  , scopeWUnBindFree
  , scopeWUnBindFree1
  )
import Blanks.Internal.Under (UnderScope (..))
import Blanks.Util.NatNewtype (NatNewtype)
import Blanks.Util.Sub (SubError)
import Control.DeepSeq (NFData (..))
import Control.Monad (ap)
import Control.Monad.Identity (Identity (..))
import Data.Sequence (Seq)

-- | A simple wrapper for your expression functor that knows how to name-bind.
-- Create free variables is 'pure', bind them with '>>=', and list free variables with folds.
-- See 'LocScope' for a version with additional annotations between layers.
newtype Scope n f a = Scope
  { unScope :: ScopeW Identity n f (Scope n f) a
  }
  deriving stock (Functor, Foldable, Traversable)

instance NatNewtype (ScopeW Identity n f (Scope n f)) (Scope n f)

instance (NFData (n (Scope n f a)), NFData a, NFData (f (Scope n f a))) => NFData (Scope n f a) where
  rnf (Scope s) = seq (rnf s) ()

pattern ScopeBound :: Int -> Scope n f a
pattern ScopeBound b = Scope (ScopeW (Identity (UnderScopeBound b)))

pattern ScopeFree :: a -> Scope n f a
pattern ScopeFree a = Scope (ScopeW (Identity (UnderScopeFree a)))

pattern ScopeAbstract :: Abstract n (Scope n f a) -> Scope n f a
pattern ScopeAbstract ab = Scope (ScopeW (Identity (UnderScopeAbstract ab)))

pattern ScopeEmbed :: f (Scope n f a) -> Scope n f a
pattern ScopeEmbed fe = Scope (ScopeW (Identity (UnderScopeEmbed fe)))

{-# COMPLETE ScopeBound, ScopeFree, ScopeAbstract, ScopeEmbed #-}

instance (IsAbstractInfo n, Functor f) => Applicative (Scope n f) where
  pure = ScopeFree
  (<*>) = ap

instance (IsAbstractInfo n, Functor f) => Monad (Scope n f) where
  return = pure
  s >>= f = scopeBind f s

instance (Eq (f (Scope n f a)), Eq (n (Scope n f a)), Eq a) => Eq (Scope n f a) where
  Scope su == Scope sv = su == sv

instance (Show (f (Scope n f a)), Show (n (Scope n f a)), Show a) => Show (Scope n f a) where
  showsPrec d (Scope (ScopeW tu)) = showString "Scope " . showParen True (showsPrec (d + 1) tu)

-- * Interface

-- | Substitution as a kind of monadic bind.
scopeBind :: (IsAbstractInfo n, Functor f) => (a -> Scope n f b) -> Scope n f a -> Scope n f b
scopeBind f = scopeWBind (Identity . f)
{-# INLINE scopeBind #-}

-- | Optional substitution as another kind of monadic bind.
scopeBindOpt :: (IsAbstractInfo n, Functor f) => (a -> Maybe (Scope n f a)) -> Scope n f a -> Scope n f a
scopeBindOpt f = scopeWBindOpt (fmap Identity . f)
{-# INLINE scopeBindOpt #-}

-- | Lift an expression functor into the scope functor.
scopeLift :: (IsAbstractInfo n, Traversable f) => f a -> Scope n f a
scopeLift = runIdentity . scopeWLift
{-# INLINE scopeLift #-}

-- | Binds free variables in an expression and returns a binder.
scopeBindFree
  :: (IsAbstractInfo n, Functor f, Eq a)
  => Seq a
  -- ^ Free variables to bind, like the names of function parameters
  -> Scope n f a
  -- ^ The expression to bind in, like the body of a function
  -> Scope n f a
scopeBindFree = scopeWBindFree
{-# INLINE scopeBindFree #-}

-- | 'scopeBindFree' for a single argument.
scopeBindFree1 :: (IsAbstractInfo n, Functor f, Eq a) => a -> Scope n f a -> Scope n f a
scopeBindFree1 = scopeWBindFree1
{-# INLINE scopeBindFree1 #-}

-- | Instantiate the bound variables in an expression with other expressions.
-- Take care to match the arity of the binder! ('scopeApply' is safer.)
scopeFillBound
  :: (IsAbstractInfo n, Functor f)
  => Seq (Scope n f a)
  -- ^ Expressions to substitute in place of bound vars
  -> Scope n f a
  -- ^ The expression to substitute in (typically a binder)
  -> Scope n f a
scopeFillBound vs = scopeWFillBound (fmap Identity vs)
{-# INLINE scopeFillBound #-}

-- | 'scopeFillBound' for a single argument.
scopeFillBound1 :: (IsAbstractInfo n, Functor f) => Scope n f a -> Scope n f a -> Scope n f a
scopeFillBound1 v = scopeWFillBound1 (Identity v)
{-# INLINE scopeFillBound1 #-}

-- | Un-bind free variables in an expression. Basically the inverse of
-- 'scopeBindFree'. Take care to match the arity of the binder! ('scopeApply' is safer.)
scopeUnBindFree
  :: (IsAbstractInfo n, Functor f)
  => Seq a
  -- ^ The names of the variables you're freeing
  -> Scope n f a
  -- ^ The expression to substitute in (typically a binder)
  -> Scope n f a
scopeUnBindFree = scopeWUnBindFree
{-# INLINE scopeUnBindFree #-}

-- 'scopeUnBindFree' for a single argument.
scopeUnBindFree1 :: (IsAbstractInfo n, Functor f) => a -> Scope n f a -> Scope n f a
scopeUnBindFree1 = scopeWUnBindFree1
{-# INLINE scopeUnBindFree1 #-}

-- | Instantiates the bound variables in an expression with other expressions.
-- Throws errors on mismatched arity, non binder expression, unbound vars, etc.
-- A version of 'scopeFillBound' that fails loudly instead of silently!
scopeApply
  :: (IsAbstractInfo n, Functor f)
  => Seq (Scope n f a)
  -- ^ Expressions to substitute in place of bound vars
  -> Scope n f a
  -- ^ The binder expression to substitute in
  -> Either SubError (Scope n f a)
scopeApply vs = scopeWApply (fmap Identity vs)
{-# INLINE scopeApply #-}

-- | 'scopeApply' for a single argument.
scopeApply1 :: (IsAbstractInfo n, Functor f) => Scope n f a -> Scope n f a -> Either SubError (Scope n f a)
scopeApply1 v = scopeWApply1 (Identity v)
{-# INLINE scopeApply1 #-}
