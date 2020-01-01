{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Blanks.Class where

import Blanks.Sub (SubError)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

class Monad m => Blanks (m :: * -> *) where
  type BlankInfo m :: *

  abstract :: Eq a => BlankInfo m -> Seq a -> m a -> m a

  abstract1 :: Eq a => BlankInfo m -> a -> m a -> m a
  abstract1 n k = abstract n (Seq.singleton k)

  unAbstract :: Seq a -> m a -> m a

  unAbstract1 :: a -> m a -> m a
  unAbstract1 = unAbstract . Seq.singleton

  instantiate :: Seq (m a) -> m a -> m a

  instantiate1 :: m a -> m a -> m a
  instantiate1 = instantiate . Seq.singleton

  apply :: Seq (m a) -> m a -> Either SubError (m a)

  apply1 :: m a -> m a -> Either SubError (m a)
  apply1 = apply . Seq.singleton
