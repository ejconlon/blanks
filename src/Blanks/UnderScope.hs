{-# LANGUAGE TemplateHaskell #-}

module Blanks.UnderScope
  ( BinderScope (..)
  , BoundScope (..)
  , EmbedScope (..)
  , FreeScope (..)
  , UnderScope (..)
  , _UnderBinderScope
  , _UnderBoundScope
  , _UnderEmbedScope
  , _UnderFreeScope
  , underScopePure
  , underScopeShift
  , underScopeBind
  -- , underScopeBindOpt
  ) where

import Control.Lens.TH (makePrisms)
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
  deriving (Eq, Show, Functor, Foldable, Traversable)

$(makePrisms ''UnderScope)

instance Functor f => Bifunctor (UnderScope n f) where
  bimap _ _ (UnderBoundScope (BoundScope b)) = UnderBoundScope (BoundScope b)
  bimap _ g (UnderFreeScope (FreeScope a)) = UnderFreeScope (FreeScope (g a))
  bimap f _ (UnderBinderScope (BinderScope i x e)) = UnderBinderScope (BinderScope i x (f e))
  bimap f _ (UnderEmbedScope (EmbedScope fe)) = UnderEmbedScope (EmbedScope (f <$> fe))

instance Foldable f => Bifoldable (UnderScope n f) where
  bifoldr _ _ z (UnderBoundScope _) = z
  bifoldr _ g z (UnderFreeScope (FreeScope a)) = g a z
  bifoldr f _ z (UnderBinderScope (BinderScope _ _ e)) = f e z
  bifoldr f _ z (UnderEmbedScope (EmbedScope fe)) = foldr f z fe

instance Traversable f => Bitraversable (UnderScope n f) where
  bitraverse _ _ (UnderBoundScope (BoundScope b)) = pure (UnderBoundScope (BoundScope b))
  bitraverse _ g (UnderFreeScope (FreeScope a)) = UnderFreeScope . FreeScope <$> g a
  bitraverse f _ (UnderBinderScope (BinderScope i x e)) = UnderBinderScope . BinderScope i x <$> f e
  bitraverse f _ (UnderEmbedScope (EmbedScope fe)) = UnderEmbedScope . EmbedScope <$> traverse f fe

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

underScopeBind :: (Applicative t, Traversable f) => (Int -> Int -> u -> u) -> (Int -> e -> (a -> t (UnderScope n f u b)) -> u) -> Int -> UnderScope n f e a -> (a -> t (UnderScope n f u b)) -> t (UnderScope n f u b)
underScopeBind recShift recBind n us f =
  case us of
    UnderBoundScope (BoundScope b) -> pure (UnderBoundScope (BoundScope b))
    UnderFreeScope (FreeScope a) -> fmap (underScopeShift recShift 0 n) (f a)
    UnderBinderScope (BinderScope i x e) -> pure (UnderBinderScope (BinderScope i x (recBind (n + i) e f)))
    UnderEmbedScope (EmbedScope fe) -> pure (UnderEmbedScope (EmbedScope (fmap (\e -> recBind n e f) fe)))

-- underScopeBindOpt ::
--   (Monad t, Functor f) =>
--   (Int -> Int -> e -> t e) ->
--   (Int -> e -> (a -> Maybe (UnderScope n f e a)) -> t e) ->
--   Int ->
--   UnderScope n f e a ->
--   (a -> Maybe (t (UnderScope n f e a))) ->
--   t (UnderScope n f e a)
-- underScopeBindOpt recShift recBindOpt n us f =
--   case us of
--     UnderBoundScope _ -> pure us
--     UnderFreeScope (FreeScope a) ->
--       case f a of
--         Nothing -> pure us
--         Just us' -> underScopeShift recShift n us'
--     UnderBinderScope (BinderScope i x e) -> UnderBinderScope (BinderScope i x (recBindOpt (n + i) e f))
--     UnderEmbedScope (EmbedScope fe) -> UnderEmbedScope (EmbedScope ((\e -> recBindOpt n e f) <$> fe))
