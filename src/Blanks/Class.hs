{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}

module Blanks.Class where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

class Monad m => Blanks (n :: *) (f :: * -> *) (m :: * -> *) | m -> n f where
  abstract :: Eq a => n -> Seq a -> m a -> m a

  abstract1 :: Eq a => n -> a -> m a -> m a
  abstract1 n k = abstract n (Seq.singleton k)
