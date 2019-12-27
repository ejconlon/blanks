{-# LANGUAGE KindSignatures #-}

module Blanks.Class where

class Blanks (b :: * -> (* -> *) -> * -> *) where
  pureBlank :: a -> b n f a
