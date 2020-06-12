module Blanks.UnderScope
  ( BinderScope (..)
  , BoundScope (..)
  , EmbedScope (..)
  , FreeScope (..)
  , UnderScope (..)
  , UnderScopeFold (..)
  , pattern UnderBoundScopePat
  , pattern UnderFreeScopePat
  , pattern UnderBinderScopePat
  , pattern UnderEmbedScopePat
  , underScopeFold
  , underScopeFoldContraMap
  , underScopePure
  , underScopeShift
  ) where

import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))

newtype BoundScope =
  BoundScope
    { unBoundScope :: Int
    }
  deriving (Eq, Show)

newtype FreeScope a =
  FreeScope
    { unFreeScope :: a
    }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data BinderScope n e =
  BinderScope
    { binderScopeArity :: !Int
    , binderScopeInfo :: !n
    , binderScopeBody :: !e
    }
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype EmbedScope f e =
  EmbedScope
    { unEmbedScope :: f e
    }
  deriving (Eq, Show, Functor)

data UnderScope n f e a
  = UnderBoundScope !BoundScope
  | UnderFreeScope !(FreeScope a)
  | UnderBinderScope !(BinderScope n e)
  | UnderEmbedScope !(EmbedScope f e)
  deriving (Eq, Show, Functor)

pattern UnderBoundScopePat :: Int -> UnderScope n f e a
pattern UnderBoundScopePat i = UnderBoundScope (BoundScope i)

pattern UnderFreeScopePat :: a -> UnderScope n f e a
pattern UnderFreeScopePat a = UnderFreeScope (FreeScope a)

pattern UnderBinderScopePat :: Int -> n -> e -> UnderScope n f e a
pattern UnderBinderScopePat i n e = UnderBinderScope (BinderScope i n e)

pattern UnderEmbedScopePat :: f e -> UnderScope n f e a
pattern UnderEmbedScopePat fe = UnderEmbedScope (EmbedScope fe)

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

underScopePure :: a -> UnderScope n f e a
underScopePure = UnderFreeScope . FreeScope

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

data UnderScopeFold n f e a r =
  UnderScopeFold
    { usfBound :: BoundScope -> r
    , usfFree :: FreeScope a -> r
    , usfBinder :: BinderScope n e -> r
    , usfEmbed :: EmbedScope f e -> r
    }
  deriving (Functor)

underScopeFold :: UnderScopeFold n f e a r -> UnderScope n f e a -> r
underScopeFold (UnderScopeFold bound free binder embed) us =
  case us of
    UnderBoundScope x -> bound x
    UnderFreeScope x -> free x
    UnderBinderScope x -> binder x
    UnderEmbedScope x -> embed x

underScopeFoldContraMap :: Functor f => (x -> e) -> UnderScopeFold n f e a r -> UnderScopeFold n f x a r
underScopeFoldContraMap f (UnderScopeFold bound free binder embed) = UnderScopeFold bound free binder' embed' where
  binder' (BinderScope r n x) = binder (BinderScope r n (f x))
  embed' (EmbedScope fx) = embed (EmbedScope (fmap f fx))

