module Blanks.Interface
  ( Blank
  , BlankLeft
  , BlankRight
  , BlankInfo
  , BlankFunctor
  , BlankPair
  , blankFree
  , blankEmbed
  , blankAbstract
  , blankAbstract1
  , blankUnAbstract
  , blankUnAbstract1
  , blankInstantiate
  , blankInstantiate1
  , blankApply
  , blankApply1
  , blankApplyThrow
  , blankApply1Throw
  , blankBind
  , blankBindOpt
  , blankLift
  , blankLiftAnno
  , blankHoistAnno
  , blankMapAnno
  ) where

import Blanks.NatNewtype (NatNewtype)
import Blanks.ScopeW
import Blanks.Sub (SubError, ThrowSub, rethrowSub)
import Data.Functor.Adjunction (Adjunction)
import Data.Kind (Type)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- | The left adjoint functor used by 'g'.
type family BlankLeft (g :: Type -> Type) :: Type -> Type

-- | The right adjoint functor used by 'g'
type family BlankRight (g :: Type -> Type) :: Type -> Type

-- | The binder info used by 'g'.
type family BlankInfo (g :: Type -> Type) :: Type

-- | The expression functor used by 'g'.
type family BlankFunctor (g :: Type -> Type) :: Type -> Type

-- | Indicates that 'g' is a "scope" functor we can use for name-binding. (Behind-the-scenes, 'g' must
-- be a newtype wrapper over the 'ScopeW' datatype.) Most of the time you will use 'Scope' or 'LocScope'
-- directly, which are instances of this class.
--
-- We use the pair of adjoint functors indexed by 'g' to shift the burden of operating in context
-- where it is more convenient. For example, 'LocScope' uses a pair of functors that are
-- essentially 'Env' and 'Reader'. The left adjoint 'Env' lets us annotate every level of our
-- expression tree with a location, and the right adjoint 'Reader' informs us of that location
-- so we don't have to make one up out of thin air!
--
-- 'Scope' uses the pair of functors 'Identity' and 'Identity', which means there is
-- no ability to store any additional information in the tree, but there's also no additional
-- burden to provide that information.
class
  ( Adjunction (BlankLeft g) (BlankRight g)
  , Applicative (BlankRight g)
  , Functor (BlankFunctor g)
  , NatNewtype (ScopeW (BlankLeft g) (BlankInfo g) (BlankFunctor g) g) g
  ) => Blank (g :: Type -> Type)

-- | A pair of 'Blank' functors that index the same info and embedded functors. Used to change adjoint functors.
type BlankPair g h = (Blank g, Blank h, BlankInfo g ~ BlankInfo h, BlankFunctor g ~ BlankFunctor h)

-- | Creates a free variable in context.
blankFree ::
  Blank g
  => a -- ^ The name of the free variable
  -> BlankRight g (g a)
blankFree = scopeWFree
{-# INLINE blankFree #-}

-- | Embeds an expression functor in context.
blankEmbed ::
  Blank g
  => BlankFunctor g (g a) -- ^ An expression
  -> BlankRight g (g a)
blankEmbed = scopeWEmbed
{-# INLINE blankEmbed #-}

-- | Binds free variables in an expression and returns a binder.
blankAbstract ::
  (Blank g, Eq a)
  => BlankInfo g -- ^ Annotation specific to your expression functor.
                 -- Might contain original variable names and types, or might
                 -- mark this as a "let" vs a "lambda".
  -> Seq a -- ^ Free variables to bind, like the names of function parameters
  -> g a -- ^ The expression to bind in, like the body of a function
  -> BlankRight g (g a)
blankAbstract = scopeWAbstract
{-# INLINE blankAbstract #-}

-- | 'blankAbstract' for a single argument.
blankAbstract1 :: (Blank g, Eq a) => BlankInfo g -> a -> g a -> BlankRight g (g a)
blankAbstract1 n k = scopeWAbstract n (Seq.singleton k)
{-# INLINE blankAbstract1 #-}

-- | Un-bind free variables in an expression. Basically the inverse of
-- 'blankAbstract'. Take care to match the arity of the binder! ('blankApply' is safer.)
blankUnAbstract ::
  Blank g
  => Seq a -- ^ The names of the variables you're freeing
  -> g a -- ^ The expression to substitutue in (typically a binder)
  -> g a
blankUnAbstract = scopeWUnAbstract
{-# INLINE blankUnAbstract #-}

-- 'blankUnAbstract' for a single argument.
blankUnAbstract1 :: Blank g => a -> g a -> g a
blankUnAbstract1 = scopeWUnAbstract . Seq.singleton
{-# INLINE blankUnAbstract1 #-}

-- | Instantiate the bound variables in an expression with other expressions.
-- Take care to match the arity of the binder! ('blankApply' is safer.)
blankInstantiate ::
  Blank g
  => Seq (BlankRight g (g a)) -- ^ Expressions to substitute in place of bound vars
  -> g a -- ^ The expression to substitute in (typically a binder)
  -> g a
blankInstantiate = scopeWInstantiate
{-# INLINE blankInstantiate #-}

-- | 'blankInstantiate' for a single argument.
blankInstantiate1 :: Blank g => BlankRight g (g a) -> g a -> g a
blankInstantiate1 = scopeWInstantiate . Seq.singleton
{-# INLINE blankInstantiate1 #-}

-- | Instantiates the bound variables in an expression with other expressions.
-- Throws errors on mismatched arity, non binder expression, unbound vars, etc.
-- A version of 'blankInstantiate' that fails loudly instead of silently!
blankApply ::
  Blank g
  => Seq (BlankRight g (g a)) -- ^ Expressions to substitute in place of bound vars
  -> g a -- ^ The binder expression to substitute in
  -> Either SubError (g a)
blankApply = scopeWApply
{-# INLINE blankApply #-}

-- | 'blankApply' for a single argument.
blankApply1 :: Blank g => BlankRight g (g a) -> g a -> Either SubError (g a)
blankApply1 = scopeWApply . Seq.singleton
{-# INLINE blankApply1 #-}

-- | A 'ThrowSub' version of 'blankApply'.
blankApplyThrow :: (Blank g, ThrowSub m, Applicative m) => Seq (BlankRight g (g a)) -> g a -> m (g a)
blankApplyThrow ks = rethrowSub . scopeWApply ks
{-# INLINE blankApplyThrow #-}

-- | A 'ThrowSub' version of 'blankApply1'.
blankApply1Throw :: (Blank g, ThrowSub m, Applicative m) => BlankRight g (g a) -> g a -> m (g a)
blankApply1Throw k = rethrowSub . scopeWApply (Seq.singleton k)
{-# INLINE blankApply1Throw #-}

-- | Substitution as a kind of monadic bind.
blankBind :: Blank g => (a -> BlankRight g (g b)) -> g a -> g b
blankBind = scopeWBind
{-# INLINE blankBind #-}

-- | Optional substitution as another kind of monadic bind.
blankBindOpt :: Blank g => (a -> Maybe (BlankRight g (g a))) -> g a -> g a
blankBindOpt = scopeWBindOpt
{-# INLINE blankBindOpt #-}

-- | Lift an expression functor into the scope functor.
blankLift :: (Blank g, Monad (BlankRight g), Traversable (BlankFunctor g)) => BlankFunctor g a -> BlankRight g (g a)
blankLift = scopeWLift
{-# INLINE blankLift #-}

-- | Lift a value of your left adjoint functor (annotating the tree) into your
-- scope functor.
blankLiftAnno :: Blank g => BlankLeft g a -> g a
blankLiftAnno = scopeWLiftAnno
{-# INLINE blankLiftAnno #-}

-- | Apply a natural transformation to your left adjoint functor (annotating the tree) to
-- change scope functors.
blankHoistAnno :: BlankPair g h => (forall x. BlankLeft g x -> BlankLeft h x) -> g a -> h a
blankHoistAnno = scopeWHoistAnno
{-# INLINE blankHoistAnno #-}

-- | Apply a function to the free variables in scope in the context of the left adjoint functor.
-- (Allows you to read annotations when fmapping.)
blankMapAnno :: Blank g => (BlankLeft g a -> BlankLeft g b) -> g a -> g b
blankMapAnno = scopeWMapAnno
{-# INLINE blankMapAnno #-}
