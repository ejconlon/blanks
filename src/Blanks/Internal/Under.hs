{-# LANGUAGE DeriveAnyClass #-}

-- | Internals.
module Blanks.Internal.Under
  ( UnderScope (..)
  , pattern UnderScopeBound
  , pattern UnderScopeFree
  , pattern UnderScopeBinder
  , pattern UnderScopeEmbed
  , underScopeShift
  ) where

import Blanks.Internal.Core (BinderScope (..), BoundScope (..), EmbedScope (..), FreeScope (..))
import Control.DeepSeq (NFData)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import GHC.Generics (Generic)

data UnderScope n f e a
  = UnderBoundScope !BoundScope
  | UnderFreeScope !(FreeScope a)
  | UnderBinderScope !(BinderScope n e)
  | UnderEmbedScope !(EmbedScope f e)
  deriving stock (Eq, Show, Functor, Generic)
  deriving anyclass (NFData)

pattern UnderScopeBound :: Int -> UnderScope n f e a
pattern UnderScopeBound i = UnderBoundScope (BoundScope i)

pattern UnderScopeFree :: a -> UnderScope n f e a
pattern UnderScopeFree a = UnderFreeScope (FreeScope a)

pattern UnderScopeBinder :: Int -> n -> e -> UnderScope n f e a
pattern UnderScopeBinder i n e = UnderBinderScope (BinderScope i n e)

pattern UnderScopeEmbed :: f e -> UnderScope n f e a
pattern UnderScopeEmbed fe = UnderEmbedScope (EmbedScope fe)

{-# COMPLETE UnderScopeBound, UnderScopeFree, UnderScopeBinder, UnderScopeEmbed #-}

instance Functor f => Bifunctor (UnderScope n f) where
  bimap _ _ (UnderBoundScope (BoundScope b)) = UnderBoundScope (BoundScope b)
  bimap _ g (UnderFreeScope (FreeScope a)) = UnderFreeScope (FreeScope (g a))
  bimap f _ (UnderBinderScope (BinderScope i x e)) = UnderBinderScope (BinderScope i x (f e))
  bimap f _ (UnderEmbedScope (EmbedScope fe)) = UnderEmbedScope (EmbedScope (fmap f fe))

instance Foldable f => Bifoldable (UnderScope n f) where
  bifoldr _ _ z (UnderBoundScope _) = z
  bifoldr _ g z (UnderFreeScope (FreeScope a)) = g a z
  bifoldr f _ z (UnderBinderScope (BinderScope _ _ e)) = f e z
  bifoldr f _ z (UnderEmbedScope (EmbedScope fe)) = foldr f z fe

instance Traversable f => Bitraversable (UnderScope n f) where
  bitraverse _ _ (UnderBoundScope (BoundScope b)) = pure (UnderBoundScope (BoundScope b))
  bitraverse _ g (UnderFreeScope (FreeScope a)) = fmap (UnderFreeScope . FreeScope) (g a)
  bitraverse f _ (UnderBinderScope (BinderScope i x e)) = fmap (UnderBinderScope . BinderScope i x) (f e)
  bitraverse f _ (UnderEmbedScope (EmbedScope fe)) = fmap (UnderEmbedScope . EmbedScope) (traverse f fe)

underScopeShift :: Functor f => (Int -> Int -> e -> e) -> Int -> Int -> UnderScope n f e a -> UnderScope n f e a
underScopeShift recShift c d us =
  case us of
    UnderBoundScope (BoundScope b) ->
      if b < c
        then us
        else UnderBoundScope (BoundScope (b + d))
    UnderFreeScope _ -> us
    UnderBinderScope (BinderScope i x e) -> UnderBinderScope (BinderScope i x (recShift (c + i) d e))
    UnderEmbedScope (EmbedScope fe) -> UnderEmbedScope (EmbedScope (fmap (recShift c d) fe))
