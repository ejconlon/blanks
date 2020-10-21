{-# LANGUAGE DeriveAnyClass #-}

-- | Utilities for gathering and caching sets of free variables.
module Blanks.Tracked
  ( Tracked (..)
  , trackedSet
  , WithTracked (..)
  , withTrackedLens
  , trackedScopeAnno
  , forgetTrackedScope
  , updateTrackedScopeWith
  , updateTrackedScope
  , trackScope
  ) where

import Blanks.Conversion (scopeAnno)
import Blanks.LocScope (LocScope, pattern LocScopeBinder, pattern LocScopeBound, pattern LocScopeEmbed,
                        pattern LocScopeFree, locScopeHoistAnno)
import Blanks.Scope (Scope)
import Control.DeepSeq (NFData)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, set)
import Lens.Micro.Extras (view)

newtype Tracked a = Tracked { unTracked :: Maybe (Set a) }
  deriving stock (Eq, Show, Foldable)
  deriving newtype (NFData)
  deriving (Semigroup, Monoid) via (Maybe (Set a))

trackedSet :: Set a -> Tracked a
trackedSet = Tracked . Just

data WithTracked a l = WithTracked
  { withTrackedState :: !(Tracked a)
  , withTrackedEnv :: !l
  } deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
    deriving anyclass (NFData)

withTrackedLens :: Lens' (WithTracked a l) (Tracked a)
withTrackedLens = lens withTrackedState (\(WithTracked _ l) t -> WithTracked t l)

trackedScopeAnno :: Functor f => LocScope l n f a -> LocScope (WithTracked a l) n f a
trackedScopeAnno = let !t = Tracked Nothing in locScopeHoistAnno (WithTracked t)

forgetTrackedScope :: Functor f => Lens' l (Tracked a) -> LocScope l n f z -> LocScope l n f z
forgetTrackedScope g = let !t = Tracked Nothing in locScopeHoistAnno (set g t)

updateTrackedScopeWith :: (Traversable f, Ord a) => (z -> a) -> Lens' l (Tracked a) -> LocScope l n f z -> (Set a, LocScope l n f z)
updateTrackedScopeWith f g = go where
  go s =
    case s of
      LocScopeBound l b ->
        case unTracked (view g l) of
          Just u -> (u, s)
          Nothing ->
            let !x = Set.empty
                !t = Tracked (Just x)
                !m = set g t l
            in (x, LocScopeBound m b)
      LocScopeFree l z ->
        case unTracked (view g l) of
          Just u -> (u, s)
          Nothing ->
            let !x = Set.singleton (f z)
                !t = Tracked (Just x)
                !m = set g t l
            in (x, LocScopeFree m z)
      LocScopeBinder l n i e ->
        case unTracked (view g l) of
          Just u -> (u, s)
          Nothing ->
            let (!x, !y) = go e
                !t = Tracked (Just x)
                !m = set g t l
            in (x, LocScopeBinder m n i y)
      LocScopeEmbed l fe ->
        case unTracked (view g l) of
          Just u -> (u, s)
          Nothing ->
            let (!x, !fy) = traverse go fe
                !t = Tracked (Just x)
                !m = set g t l
            in (x, LocScopeEmbed m fy)

updateTrackedScope :: (Traversable f, Ord a) => Lens' l (Tracked a) -> LocScope l n f a -> (Set a, LocScope l n f a)
updateTrackedScope = updateTrackedScopeWith id

trackScope :: (Traversable f, Ord a) => Scope n f a -> (Set a, LocScope (Tracked a) n f a)
trackScope = let !t = Tracked Nothing in updateTrackedScope id . scopeAnno t
