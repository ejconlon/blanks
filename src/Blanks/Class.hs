{-# LANGUAGE FlexibleContexts #-}

module Blanks.Class where

import Blanks.Sub (SubError)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

type family BlankInfo (m :: * -> *) :: *
type family BlankFunctor (m :: * -> *) :: * -> *

class BlankEmbed (m :: * -> *) where
  blankEmbed :: BlankFunctor m (m a) -> m a

class BlankAbstract (m :: * -> *) where
  blankFree :: a -> m a

  blankAbstract :: Eq a => BlankInfo m -> Seq a -> m a -> m a

  blankAbstract1 :: Eq a => BlankInfo m -> a -> m a -> m a
  blankAbstract1 n k = blankAbstract n (Seq.singleton k)

  blankUnAbstract :: Seq a -> m a -> m a

  blankUnAbstract1 :: a -> m a -> m a
  blankUnAbstract1 = blankUnAbstract . Seq.singleton

  blankInstantiate :: Seq (m a) -> m a -> m a

  blankInstantiate1 :: m a -> m a -> m a
  blankInstantiate1 = blankInstantiate . Seq.singleton

  blankApply :: Seq (m a) -> m a -> Either SubError (m a)

  blankApply1 :: m a -> m a -> Either SubError (m a)
  blankApply1 = blankApply . Seq.singleton

