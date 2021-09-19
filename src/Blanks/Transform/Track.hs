{-# LANGUAGE DeriveAnyClass #-}

-- | Utilities for gathering and caching sets of free variables.
module Blanks.Transform.Track
  ( Tracked (..)
  , trackedIsEmpty
  , mkTrackedFree
  , mkTrackedBound
  , shiftTracked
  , WithTracked
  , forgetTrackedScope
  , trackScope
  , trackScopeSimple
  ) where

import Blanks.Conversion (scopeAnno)
import Blanks.Internal.Abstract (Abstract (..), IsAbstractInfo (..))
import Blanks.LocScope (LocScope, locScopeHoistAnno, pattern LocScopeAbstract, pattern LocScopeBound,
                        pattern LocScopeEmbed, pattern LocScopeFree)
import Blanks.Scope (Scope)
import Blanks.Util.Located (Located (..))
import Control.DeepSeq (NFData)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)

-- | Tracked sets of free and bound variables. Works as a 'Semigroup' or 'Monoid'
-- and can build built from smart constructors 'mkTrackedFree' and 'mkTrackedBound'.
data Tracked a = Tracked
  { trackedFree :: !(Set a)
  , trackedBound :: !(Set Int)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

trackedIsEmpty :: Tracked a -> Bool
trackedIsEmpty (Tracked f b) = Set.null f && Set.null b

-- | Smart constructor for a single free variable.
mkTrackedFree :: a -> Tracked a
mkTrackedFree a = Tracked (Set.singleton a) Set.empty

-- | Smart constructor for a single bound variable.
mkTrackedBound :: Int -> Tracked a
mkTrackedBound b = Tracked Set.empty (Set.singleton b)

-- | Shifts bound variables DOWN by the given amount.
shiftTracked :: Int -> Tracked a -> Tracked a
shiftTracked i t@(Tracked f b) =
  if Set.null b
    then t
    else
      let !b' = if Set.findMax b < i then Set.empty else Set.dropWhileAntitone (< 0) (Set.mapMonotonic (\x -> x - i) b)
      in Tracked f b'

instance Ord a => Semigroup (Tracked a) where
  Tracked f1 b1 <> Tracked f2 b2 = Tracked (Set.union f1 f2) (Set.union b1 b2)

instance Ord a => Monoid (Tracked a) where
  mempty = Tracked Set.empty Set.empty
  mappend = (<>)

type WithTracked a = Located (Tracked a)

-- | Drop tracked annotations from the scope tree.
forgetTrackedScope :: (Functor n, Functor f) => LocScope (WithTracked a l) n f z -> LocScope l n f z
forgetTrackedScope = locScopeHoistAnno locatedVal

trackScopeInner :: (IsAbstractInfo n, Traversable f, Ord a) => LocScope l n f a -> (Tracked a, LocScope (WithTracked a l) n f a)
trackScopeInner s =
  case s of
    LocScopeBound l b ->
      let !t = Tracked Set.empty (Set.singleton b)
          !m = Located t l
      in (t, LocScopeBound m b)
    LocScopeFree l a ->
      let !t = Tracked (Set.singleton a) Set.empty
          !m = Located t l
      in (t, LocScopeFree m a)
    LocScopeAbstract l (Abstract info body) ->
      let (!t0, !body') = trackScopeInner body
          (!t1, !info') = traverse trackScopeInner info
          !t = shiftTracked (abstractInfoArity info) (t1 <> t0)
          !m = Located t l
      in (t, LocScopeAbstract m (Abstract info' body'))
    LocScopeEmbed l fe ->
      let (!t, !fy) = traverse trackScopeInner fe
          !m = Located t l
      in (t, LocScopeEmbed m fy)

-- | Track variables as bottom-up annotations on the annotated scope tree.
trackScope :: (IsAbstractInfo n, Traversable f, Ord a) => LocScope l n f a -> LocScope (WithTracked a l) n f a
trackScope = snd . trackScopeInner

-- | Track variables as bottom-up annotations on the given pure scope tree.
trackScopeSimple :: (IsAbstractInfo n, Traversable f, Ord a) => Scope n f a -> LocScope (WithTracked a ()) n f a
trackScopeSimple = trackScope . scopeAnno ()
