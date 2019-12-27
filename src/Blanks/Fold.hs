module Blanks.Fold
  ( ScopeFold (..)
  , boundFold
  , foldScope
  ) where

import Blanks.Scope
import Blanks.Sub
import Blanks.UnderScope

data ScopeFold n f a r =
  ScopeFold
    { sfBound :: BoundScope -> r
    , sfFree :: FreeScope a -> r
    , sfBinder :: BinderScope n (Scope n f a) -> r
    , sfEmbed :: EmbedScope f (Scope n f a) -> r
    }
  deriving (Functor)

boundFold ::
     ThrowSub m
  => (FreeScope a -> m r)
  -> (BinderScope n (Scope n f a) -> m r)
  -> (EmbedScope f (Scope n f a) -> m r)
  -> ScopeFold n f a (m r)
boundFold = ScopeFold (throwSub . UnboundError . unBoundScope)

foldScope :: ScopeFold n f a r -> Scope n f a -> r
foldScope (ScopeFold bound free binder embed) (Scope us) =
  case us of
    UnderBoundScope u -> bound u
    UnderFreeScope u -> free u
    UnderBinderScope u -> binder u
    UnderEmbedScope u -> embed u
