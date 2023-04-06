{-# LANGUAGE DeriveAnyClass #-}

-- | Utilities to bind global references so they aren't closed over as free in subsequent operations.
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
  )
where

import Blanks.LocScope
  ( LocScope
  , pattern LocScopeAbstract
  , pattern LocScopeBound
  , pattern LocScopeEmbed
  , pattern LocScopeFree
  )
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- | To "hide" free vars referring to globals, we rewrite the scope tree with a new functor that includes global refs.
data GlobalFunctor g f a
  = GlobalFunctorBase !(f a)
  | GlobalFunctorGlobal !g
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

-- | Splits the given functor into base and global. Used to un-rewrite - see 'unGlobalScope'.
class MatchGlobal f g h | f -> g h where
  matchGlobal :: f a -> GlobalFunctor g h a

instance MatchGlobal (GlobalFunctor g f) g f where
  matchGlobal = id

-- | Is a var global or just plain free? (Glorified boolean.)
data VarClass g a
  = VarClassGlobal !g
  | VarClassFree !a
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

-- | Function to transform and classify free vars as global or not.
type VarClassifier g a z = a -> VarClass g z

-- | A 'VarClassifier' that classifies everything as free. (A no-op for 'globalScope'.)
nullClassifier :: VarClassifier g a a
nullClassifier = VarClassFree

-- | A 'VarClassifier' that classifies vars as global if the predicate is satisfied.
predClassifier :: (a -> Bool) -> VarClassifier a a a
predClassifier p a = if p a then VarClassGlobal a else VarClassFree a

-- | Rewrites the scope tree with the given classification of global vs free vars.
globalScope :: (Functor n, Functor f) => VarClassifier g a z -> LocScope l n f a -> LocScope l n (GlobalFunctor g f) z
globalScope cfier = go
 where
  go s =
    case s of
      LocScopeBound l b -> LocScopeBound l b
      LocScopeFree l a ->
        case cfier a of
          VarClassGlobal g -> LocScopeEmbed l (GlobalFunctorGlobal g)
          VarClassFree z -> LocScopeFree l z
      LocScopeAbstract l ab -> LocScopeAbstract l (fmap go ab)
      LocScopeEmbed l fe -> LocScopeEmbed l (GlobalFunctorBase (fmap go fe))

-- | The inverse of 'VarClassifier' - unifies global and free vars.
type VarUnClassifier g z a = VarClass g z -> a

-- | A 'VarUnClassifier' that simply merges global and free vars. (A no-op for 'unGlobalScope' if no globals are present.)
nullUnClassifier :: VarUnClassifier a a a
nullUnClassifier c =
  case c of
    VarClassGlobal a -> a
    VarClassFree a -> a

-- | Un-rewrites the scope tree to unify global and free vars as free.
unGlobalScope :: (Functor n, Functor h, MatchGlobal f g h) => VarUnClassifier g z a -> LocScope l n f z -> LocScope l n h a
unGlobalScope unCfier = go
 where
  go s =
    case s of
      LocScopeBound l b -> LocScopeBound l b
      LocScopeFree l a -> LocScopeFree l (unCfier (VarClassFree a))
      LocScopeAbstract l ab -> LocScopeAbstract l (fmap go ab)
      LocScopeEmbed l fe ->
        case matchGlobal fe of
          GlobalFunctorGlobal g -> LocScopeFree l (unCfier (VarClassGlobal g))
          GlobalFunctorBase he -> LocScopeEmbed l (fmap go he)
