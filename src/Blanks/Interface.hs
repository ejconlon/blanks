{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Blanks.Interface where

import Blanks.ScopeW
import Blanks.Sub (SubError)
import Blanks.NatNewtype (NatNewtype)
import Data.Functor.Adjunction (Adjunction)
import Data.Kind (Type)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

type family BlankLeft (g :: Type -> Type) :: Type -> Type
type family BlankRight (g :: Type -> Type) :: Type -> Type
type family BlankInfo (g :: Type -> Type) :: Type
type family BlankFunctor (g :: Type -> Type) :: Type -> Type

type BlankC (g :: Type -> Type) =
  ( Adjunction (BlankLeft g) (BlankRight g)
  , Applicative (BlankRight g)
  , Functor (BlankFunctor g)
  , NatNewtype (ScopeW (BlankLeft g) (BlankInfo g) (BlankFunctor g) g) g
  )

class BlankC g => Blank (g :: Type -> Type)

-- | "free name"
blankFree :: Blank g => a -> BlankRight g (g a)
blankFree = scopeWFree

-- | "embed functor"
blankEmbed :: Blank g => BlankFunctor g (g a) -> BlankRight g (g a)
blankEmbed = scopeWEmbed

-- | "abstract info names body"
blankAbstract :: (Blank g, Eq a) => BlankInfo g -> Seq a -> g a -> BlankRight g (g a)
blankAbstract = scopeWAbstract

-- | 'blankAbstract' for a single argument
blankAbstract1 :: (Blank g, Eq a) => BlankInfo g -> a -> g a -> BlankRight g (g a)
blankAbstract1 n k = blankAbstract n (Seq.singleton k)

-- | "unAbstract names body"
blankUnAbstract :: Blank g => Seq a -> g a -> g a
blankUnAbstract = scopeWUnAbstract

-- 'blankUnAbstract' for a single argument
blankUnAbstract1 :: Blank g => a -> g a -> g a
blankUnAbstract1 = blankUnAbstract . Seq.singleton

-- | "instantiate args body"
blankInstantiate :: Blank g => Seq (BlankRight g (g a)) -> g a -> g a
blankInstantiate = scopeWInstantiate

-- | 'blankInstantiate' for a single argument
blankInstantiate1 :: Blank g => BlankRight g (g a) -> g a -> g a
blankInstantiate1 = blankInstantiate . Seq.singleton

-- | "apply args abstraction"
blankApply :: Blank g => Seq (BlankRight g (g a)) -> g a -> Either SubError (g a)
blankApply = scopeWApply

-- | 'blankApply' for a single argument
blankApply1 :: Blank g => BlankRight g (g a) -> g a -> Either SubError (g a)
blankApply1 = blankApply . Seq.singleton

-- | Substitution as a kind of monadic bind
blankBind :: Blank g => (a -> BlankRight g (g b)) -> g a -> g b
blankBind = scopeWBind

-- | Optional substitution
blankBindOpt :: Blank g => (a -> Maybe (BlankRight g (g a))) -> g a -> g a
blankBindOpt = scopeWBindOpt

-- -- | Raw fold
-- blankRawFold :: BlankRawFold m a r -> m a -> BlankDomain m r

-- -- | Fold
-- blankFold :: BlankFold m a r -> m a -> r

-- -- | Lift annotation
-- blankLiftAnno :: BlankDomain m a -> m a
