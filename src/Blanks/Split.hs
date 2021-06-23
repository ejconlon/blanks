{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Utilities to lambda-lift and closure-conv
module Blanks.Split
  ( BinderId (..)
  , SplitFunctor (..)
  , SplitBinder (..)
  , SplitResult (..)
  , splitLocScope
  ) where

import Blanks.Core (BinderScope (..))
import Blanks.LocScope (LocScope, pattern LocScopeBinder, pattern LocScopeBound, pattern LocScopeEmbed,
                        pattern LocScopeFree)
import Blanks.Tracked (Tracked (..), WithTracked (..), mkTrackedBound, mkTrackedFree)
import Control.DeepSeq (NFData (..))
import Control.Monad.State (State, gets, modify', runState)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)

data Stream x = Stream !x (Stream x)

enumStreamFrom :: Enum e => e -> Stream e
enumStreamFrom e = Stream e (enumStreamFrom (succ e))

enumStream :: Enum e => Stream e
enumStream = enumStreamFrom (toEnum 0)

newtype BinderId = BinderId { unBinderId :: Int }
  deriving stock (Eq, Show, Ord)
  deriving newtype (Enum, NFData, Num)

-- TODO Maybe include `SplitFunctorGlobal !a`
data SplitFunctor f a =
    SplitFunctorBase !(f a)
  | SplitFunctorClosure !BinderId !(Seq Int)
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

data SplitBinder l n f a = SplitBinder
  { splitBinderClosureArity :: !Int
  , splitBinderFree :: !(Set a)
  , splitBinderScope :: !(BinderScope n (LocScope l n (SplitFunctor f) a))
  } deriving stock (Generic)

instance (Eq l, Eq n, Eq a, Eq (f (LocScope l n (SplitFunctor f) a))) => Eq (SplitBinder l n f a) where
  SplitBinder a1 f1 s1 == SplitBinder a2 f2 s2 = a1 == a2 && f1 == f2 && s1 == s2

instance (Show l, Show n, Show a, Show (f (LocScope l n (SplitFunctor f) a))) => Show (SplitBinder l n f a) where
  showsPrec d (SplitBinder a f s) = showString "SplitBinder " . showsPrec (d+1) a . showChar ' ' . showsPrec (d+1) f . showChar ' ' . showsPrec (d+1) s

instance (NFData l, NFData n, NFData a, NFData (f (LocScope l n (SplitFunctor f) a))) => NFData (SplitBinder l n f a) where
  rnf (SplitBinder a f s) = seq (rnf a) (seq (rnf f) (rnf s))

data SplitState l n f a = SplitState
  { splitStateStream :: !(Stream BinderId)
  , splitStateBinders :: !(Map BinderId (SplitBinder l n f a))
  }

initSplitState :: SplitState l n f a
initSplitState = SplitState enumStream Map.empty

getNextBinderId :: State (SplitState l n f a) BinderId
getNextBinderId = do
  st <- gets splitStateStream
  let Stream hd tl = st
  modify' (\ss -> ss { splitStateStream = tl })
  pure hd

insertBinder :: BinderId -> SplitBinder l n f a -> State (SplitState l n f a) ()
insertBinder bid lb = modify' (\ss -> ss { splitStateBinders = Map.insert bid lb (splitStateBinders ss) })

remapBound :: Int -> Set Int -> Int -> Int
remapBound r bs b = maybe b (+ r) (Set.lookupIndex b bs)

splitLocScopeInner :: (Traversable f, Ord a) => Int -> Set Int -> LocScope (WithTracked a l) n f a -> State (SplitState l n f a) (Tracked a, LocScope l n (SplitFunctor f) a)
splitLocScopeInner r bs ls =
  case ls of
    LocScopeBound (WithTracked _ l) b ->
      let !b' = remapBound r bs b
      in pure (mkTrackedBound b', LocScopeBound l b')
    LocScopeFree (WithTracked _ l) a ->
      pure (mkTrackedFree a, LocScopeFree l a)
    LocScopeEmbed (WithTracked _ l) fe -> fmap (\fx -> let (!t, !fe') = sequence fx in (t, LocScopeEmbed l (SplitFunctorBase fe'))) (traverse (splitLocScopeInner r bs) fe)
    LocScopeBinder (WithTracked (Tracked fv bv) l) x n e -> do
      bid <- getNextBinderId
      let bs' = Set.mapMonotonic (+ r) bv
      (_, e') <- splitLocScopeInner x bs' e
      let lb = SplitBinder (Set.size bs') fv (BinderScope x n e')
      insertBinder bid lb
      let ss = Seq.fromList (Set.toList bv)
      pure (Tracked Set.empty bv, LocScopeEmbed l (SplitFunctorClosure bid ss))

data SplitResult l n f a = SplitResult
  { splitResultTracked :: !(Tracked a)
  , splitResultScope :: !(LocScope l n (SplitFunctor f) a)
  , splitResultBinders :: !(Map BinderId (SplitBinder l n f a))
  }

instance (Eq (f (LocScope l n (SplitFunctor f) a)), Eq l, Eq n, Eq a) => Eq (SplitResult l n f a) where
  SplitResult t1 s1 b1 == SplitResult t2 s2 b2 = t1 == t2 && s1 == s2 && b1 == b2

instance (Show (f (LocScope l n (SplitFunctor f) a)), Show l, Show n, Show a) => Show (SplitResult l n f a) where
  showsPrec d (SplitResult t s b) = showString "SplitResult " . showsPrec (d+1) t . showsPrec (d+1) s . showsPrec (d+1) b

-- TODO Take predicate on `n` to determine which binders need to be converted.
-- TODO Take `a -> Maybe a` to resolve globals
splitLocScope :: (Traversable f, Ord a) => LocScope (WithTracked a l) n f a -> SplitResult l n f a
splitLocScope s =
  let ((!t, !s'), !ss) = runState (splitLocScopeInner 0 Set.empty s) initSplitState
  in SplitResult t s' (splitStateBinders ss)
