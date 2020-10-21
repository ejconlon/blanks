{-# LANGUAGE DeriveAnyClass #-}

-- | Internals.
module Blanks.Core
  ( BoundScope (..)
  , FreeScope (..)
  , BinderScope (..)
  , EmbedScope (..)
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

newtype BoundScope =
  BoundScope
    { unBoundScope :: Int
    }
  deriving newtype (Eq, Show, NFData)

newtype FreeScope a =
  FreeScope
    { unFreeScope :: a
    }
  deriving stock (Eq, Show, Functor, Foldable, Traversable)
  deriving newtype (NFData)

data BinderScope n e =
  BinderScope
    { binderScopeArity :: !Int
    , binderScopeInfo :: !n
    , binderScopeBody :: e
    }
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData)

newtype EmbedScope f e =
  EmbedScope
    { unEmbedScope :: f e
    }
  deriving newtype (Eq, Show, Functor, NFData)
