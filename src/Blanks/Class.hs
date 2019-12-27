{-# LANGUAGE KindSignatures #-}

module Blanks.Class where

class Blanks (m :: * -> (* -> *) -> * -> *) where
  pureBlanks :: a -> m n f a
  bindBlanks :: Traversable f => m n f a -> (a -> m n f b) -> m n f b
