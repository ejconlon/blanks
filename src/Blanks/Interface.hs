module Blanks.Interface
  ( Blank
  , BlankLeft
  , BlankRight
  , BlankInfo
  , BlankFunctor
  , BlankRawFold
  , BlankFold
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
  , blankRawFold
  , blankFold
  , blankLiftAnno
  , blankHoistAnno
  , blankMapAnno
  ) where

import Blanks.NatNewtype (NatNewtype)
import Blanks.ScopeW
import Blanks.Sub (SubError, ThrowSub, rethrowSub)
import Blanks.UnderScope (UnderScopeFold)
import Data.Functor.Adjunction (Adjunction)
import Data.Kind (Type)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

type family BlankLeft (g :: Type -> Type) :: Type -> Type
type family BlankRight (g :: Type -> Type) :: Type -> Type
type family BlankInfo (g :: Type -> Type) :: Type
type family BlankFunctor (g :: Type -> Type) :: Type -> Type

type BlankRawFold (g :: Type -> Type) (a :: Type) (r :: Type) = UnderScopeFold (BlankInfo g) (BlankFunctor g) (g a) a r
type BlankFold (g :: Type -> Type) (a :: Type) (r :: Type) = BlankRawFold g a (BlankRight g r)

type BlankC (g :: Type -> Type) =
  ( Adjunction (BlankLeft g) (BlankRight g)
  , Applicative (BlankRight g)
  , Functor (BlankFunctor g)
  , NatNewtype (ScopeW (BlankLeft g) (BlankInfo g) (BlankFunctor g) g) g
  )

class BlankC g => Blank (g :: Type -> Type)

type BlankPair g h = (Blank g, Blank h, BlankInfo g ~ BlankInfo h, BlankFunctor g ~ BlankFunctor h)

-- | "free name"
blankFree :: Blank g => a -> BlankRight g (g a)
blankFree = scopeWFree
{-# INLINE blankFree #-}

-- | "embed functor"
blankEmbed :: Blank g => BlankFunctor g (g a) -> BlankRight g (g a)
blankEmbed = scopeWEmbed
{-# INLINE blankEmbed #-}

-- | "abstract info names body"
blankAbstract :: (Blank g, Eq a) => BlankInfo g -> Seq a -> g a -> BlankRight g (g a)
blankAbstract = scopeWAbstract
{-# INLINE blankAbstract #-}

-- | 'blankAbstract' for a single argument
blankAbstract1 :: (Blank g, Eq a) => BlankInfo g -> a -> g a -> BlankRight g (g a)
blankAbstract1 n k = scopeWAbstract n (Seq.singleton k)
{-# INLINE blankAbstract1 #-}

-- | "unAbstract names body"
blankUnAbstract :: Blank g => Seq a -> g a -> g a
blankUnAbstract = scopeWUnAbstract
{-# INLINE blankUnAbstract #-}

-- 'blankUnAbstract' for a single argument
blankUnAbstract1 :: Blank g => a -> g a -> g a
blankUnAbstract1 = scopeWUnAbstract . Seq.singleton
{-# INLINE blankUnAbstract1 #-}

-- | "instantiate args body"
blankInstantiate :: Blank g => Seq (BlankRight g (g a)) -> g a -> g a
blankInstantiate = scopeWInstantiate
{-# INLINE blankInstantiate #-}

-- | 'blankInstantiate' for a single argument
blankInstantiate1 :: Blank g => BlankRight g (g a) -> g a -> g a
blankInstantiate1 = scopeWInstantiate . Seq.singleton
{-# INLINE blankInstantiate1 #-}

-- | "apply args abstraction"
blankApply :: Blank g => Seq (BlankRight g (g a)) -> g a -> Either SubError (g a)
blankApply = scopeWApply
{-# INLINE blankApply #-}

-- | 'blankApply' for a single argument
blankApply1 :: Blank g => BlankRight g (g a) -> g a -> Either SubError (g a)
blankApply1 = scopeWApply . Seq.singleton
{-# INLINE blankApply1 #-}

-- | 'ThrowSub' version of 'blankApply'
blankApplyThrow :: (Blank g, ThrowSub m, Applicative m) => Seq (BlankRight g (g a)) -> g a -> m (g a)
blankApplyThrow ks = rethrowSub . scopeWApply ks
{-# INLINE blankApplyThrow #-}

-- | 'ThrowSub' version of 'blankApply1'
blankApply1Throw :: (Blank g, ThrowSub m, Applicative m) => BlankRight g (g a) -> g a -> m (g a)
blankApply1Throw k = rethrowSub . scopeWApply (Seq.singleton k)
{-# INLINE blankApply1Throw #-}

-- | Substitution as a kind of monadic bind
blankBind :: Blank g => (a -> BlankRight g (g b)) -> g a -> g b
blankBind = scopeWBind
{-# INLINE blankBind #-}

-- | Optional substitution
blankBindOpt :: Blank g => (a -> Maybe (BlankRight g (g a))) -> g a -> g a
blankBindOpt = scopeWBindOpt
{-# INLINE blankBindOpt #-}

-- | Raw fold
blankRawFold :: Blank g => BlankRawFold g a r -> g a -> BlankLeft g r
blankRawFold = scopeWRawFold
{-# INLINE blankRawFold #-}

-- | Fold
blankFold :: Blank g => BlankFold g a r -> g a -> r
blankFold = scopeWFold
{-# INLINE blankFold #-}

-- | Lift annotation
blankLiftAnno :: Blank g => BlankLeft g a -> g a
blankLiftAnno = scopeWLiftAnno
{-# INLINE blankLiftAnno #-}

-- | Hoist annotation
blankHoistAnno :: BlankPair g h => (forall x. BlankLeft g x -> BlankLeft h x) -> g a -> h a
blankHoistAnno = scopeWHoistAnno
{-# INLINE blankHoistAnno #-}

-- | Map annotation
blankMapAnno :: Blank g => (BlankLeft g a -> BlankLeft g b) -> g a -> g b
blankMapAnno = scopeWMapAnno
{-# INLINE blankMapAnno #-}
