{-# LANGUAGE DeriveAnyClass #-}

-- | Utilities to bind global references so they're not closed over as free in subsequent operations.
module Blanks.Transform.Global
  ( GlobalFunctor (..)
  , MatchGlobal (..)
  , VarClass (..)
  , VarClassifier
  , nullClassifier
  , predClassifier
  , globalScope
  , VarUnClassifier
  , nullUnClassifier
  , unGlobalScope
  ) where

import Blanks.LocScope (LocScope, pattern LocScopeBinder, pattern LocScopeBound, pattern LocScopeEmbed,
                        pattern LocScopeFree)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data GlobalFunctor g f a =
    GlobalFunctorBase !(f a)
  | GlobalFunctorGlobal !g
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

class MatchGlobal f g h | f -> g h where
  matchGlobal :: f a -> GlobalFunctor g h a

instance MatchGlobal (GlobalFunctor g f) g f where
  matchGlobal = id

data VarClass g a =
    VarClassGlobal !g
  | VarClassFree !a
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

type VarClassifier g a z = a -> VarClass g z

nullClassifier :: VarClassifier g a a
nullClassifier = VarClassFree

predClassifier :: (a -> Bool) -> VarClassifier a a a
predClassifier p a = if p a then VarClassGlobal a else VarClassFree a

globalScope :: Functor f => VarClassifier g a z -> LocScope l n f a -> LocScope l n (GlobalFunctor g f) z
globalScope cfier = go where
  go s =
    case s of
      LocScopeBound l b -> LocScopeBound l b
      LocScopeFree l a ->
        case cfier a of
          VarClassGlobal g -> LocScopeEmbed l (GlobalFunctorGlobal g)
          VarClassFree z -> LocScopeFree l z
      LocScopeBinder l n i e -> LocScopeBinder l n i (go e)
      LocScopeEmbed l fe -> LocScopeEmbed l (GlobalFunctorBase (fmap go fe))

type VarUnClassifier g z a = VarClass g z -> a

nullUnClassifier :: VarUnClassifier a a a
nullUnClassifier c =
  case c of
    VarClassGlobal a -> a
    VarClassFree a -> a

unGlobalScope :: (Functor h, MatchGlobal f g h) => VarUnClassifier g z a -> LocScope l n f z -> LocScope l n h a
unGlobalScope unCfier = go where
  go s =
    case s of
      LocScopeBound l b -> LocScopeBound l b
      LocScopeFree l a -> LocScopeFree l (unCfier (VarClassFree a))
      LocScopeBinder l n i e -> LocScopeBinder l n i (go e)
      LocScopeEmbed l fe ->
        case matchGlobal fe of
          GlobalFunctorGlobal g -> LocScopeFree l (unCfier (VarClassGlobal g))
          GlobalFunctorBase he -> LocScopeEmbed l (fmap go he)
