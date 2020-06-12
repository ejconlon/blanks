{-# LANGUAGE FlexibleContexts #-}

module Blanks.Class
  ( BlankAbstract (..)
  , BlankCodomain
  , BlankEmbed (..)
  , BlankFunctor
  , BlankInfo
  ) where

import Blanks.Sub (SubError)
import Data.Kind (Type)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

type family BlankInfo (m :: Type -> Type) :: Type
type family BlankFunctor (m :: Type -> Type) :: Type -> Type
type family BlankCodomain (m :: Type -> Type) :: Type -> Type

class BlankEmbed (m :: Type -> Type) where
  -- | "embed functor"
  blankEmbed :: BlankFunctor m (m a) -> BlankCodomain m (m a)

class BlankAbstract (m :: Type -> Type) where
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
