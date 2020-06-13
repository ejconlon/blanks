{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Blanks.Class
  ( Blank (..)
  , BlankDomain
  , BlankCodomain
  , BlankEmbedded
  , BlankFunctor
  , BlankInfo
  , BlankRawFold
  , BlankFold
  ) where

import Blanks.RightAdjunct (RightAdjunct)
import Blanks.Sub (SubError)
import Blanks.UnderScope (UnderScopeFold)
import Data.Kind (Type)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

type family BlankDomain (m :: Type -> Type) :: Type -> Type
type BlankCodomain (m :: Type -> Type) = RightAdjunct (BlankDomain m)
type family BlankInfo (m :: Type -> Type) :: Type
type family BlankFunctor (m :: Type -> Type) :: Type -> Type
type family BlankEmbedded (m :: Type -> Type) :: Type -> Type

type BlankRawFold m a r = UnderScopeFold (BlankInfo m) (BlankFunctor m) (BlankEmbedded m a) a r
type BlankFold m a r = BlankRawFold m a (BlankCodomain m r)

class Blank (m :: Type -> Type) where
  -- | "free name"
  blankFree :: a -> BlankCodomain m (m a)

  -- | "abstract info names body"
  blankAbstract :: Eq a => BlankInfo m -> Seq a -> m a -> BlankCodomain m (m a)

  -- | 'blankAbstract' for a single argument
  blankAbstract1 :: Eq a => BlankInfo m -> a -> m a -> BlankCodomain m (m a)
  blankAbstract1 n k = blankAbstract n (Seq.singleton k)

  -- | "unAbstract names body"
  blankUnAbstract :: Seq a -> m a -> m a

  -- 'blankUnAbstract' for a single argument
  blankUnAbstract1 :: a -> m a -> m a
  blankUnAbstract1 = blankUnAbstract . Seq.singleton

  -- | "instantiate args body"
  blankInstantiate :: Seq (BlankCodomain m (m a)) -> m a -> m a

  -- | 'blankInstantiate' for a single argument
  blankInstantiate1 :: BlankCodomain m (m a) -> m a -> m a
  blankInstantiate1 = blankInstantiate . Seq.singleton

  -- | "apply args abstraction"
  blankApply :: Seq (BlankCodomain m (m a)) -> m a -> Either SubError (m a)

  -- | 'blankApply' for a single argument
  blankApply1 :: BlankCodomain m (m a) -> m a -> Either SubError (m a)
  blankApply1 = blankApply . Seq.singleton

  -- | Substitution as a kind of monadic bind
  blankBind :: (a -> BlankCodomain m (m b)) -> m a -> m b

  -- | Optional substitution
  blankBindOpt :: (a -> Maybe (BlankCodomain m (m a))) -> m a -> m a

  -- | "embed functor"
  blankEmbed :: BlankFunctor m (BlankEmbedded m a) -> BlankCodomain m (m a)

  -- | Raw fold
  blankRawFold :: BlankRawFold m a r -> m a -> BlankDomain m r

  -- | Fold
  blankFold :: BlankFold m a r -> m a -> r

  -- | Lift annotation
  blankLiftAnno :: BlankDomain m a -> m a
