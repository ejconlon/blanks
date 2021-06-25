{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Utilities to lambda-lift and closure-convert expressions.
module Blanks.Split
  ( BinderId (..)
  , SplitFunctor (..)
  , SplitBinder (..)
  , SplitResult
  , SplitState (..)
  , emptySplitState
  , splitLocScope
  ) where

import Blanks.Core (BinderScope (..))
import Blanks.Global (GlobalFunctor (..), MatchGlobal (..))
import Blanks.LocScope (LocScope, pattern LocScopeBinder, pattern LocScopeBound, pattern LocScopeEmbed,
                        pattern LocScopeFree)
import Blanks.Tracked (Tracked (..), WithTracked (..), mkTrackedBound, mkTrackedFree)
import Control.DeepSeq (NFData (..))
import Control.Monad.State.Strict (State, gets, modify')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)

newtype BinderId = BinderId { unBinderId :: Int }
  deriving stock (Eq, Show, Ord)
  deriving newtype (Enum, NFData, Num)

data SplitFunctor f a =
    SplitFunctorBase !(f a)
  | SplitFunctorClosure !BinderId !(Seq Int)
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

instance MatchGlobal f g h => MatchGlobal (SplitFunctor f) g (SplitFunctor h) where
  matchGlobal fa =
    case fa of
      SplitFunctorBase fa' ->
        case matchGlobal fa' of
          GlobalFunctorGlobal g -> GlobalFunctorGlobal g
          GlobalFunctorBase ha -> GlobalFunctorBase (SplitFunctorBase ha)
      SplitFunctorClosure x ys -> GlobalFunctorBase (SplitFunctorClosure x ys)

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

type SplitResult l n f a = WithTracked a (LocScope l n (SplitFunctor f) a)

data SplitState l n f a = SplitState
  { splitStateNextId :: !BinderId
  , splitStateBinders :: !(Map BinderId (SplitBinder l n f a))
  } deriving stock (Generic)

instance (Eq l, Eq n, Eq a, Eq (f (LocScope l n (SplitFunctor f) a))) => Eq (SplitState l n f a) where
  SplitState n1 m1 == SplitState n2 m2 = n1 == n2 && m1 == m2

instance (Show l, Show n, Show a, Show (f (LocScope l n (SplitFunctor f) a))) => Show (SplitState l n f a) where
  showsPrec d (SplitState n m) = showString "SplitState " . showsPrec (d+1) n . showChar ' ' . showsPrec (d+1) m

instance (NFData l, NFData n, NFData a, NFData (f (LocScope l n (SplitFunctor f) a))) => NFData (SplitState l n f a) where
  rnf (SplitState n m) = seq (rnf n) (rnf m)

emptySplitState :: SplitState l n f a
emptySplitState = SplitState (toEnum 0) Map.empty

allocBinderId :: State (SplitState l n f a) BinderId
allocBinderId = do
  next <- gets splitStateNextId
  modify' (\ss -> ss { splitStateNextId = succ next })
  pure next

insertBinder :: BinderId -> SplitBinder l n f a -> State (SplitState l n f a) ()
insertBinder bid lb = modify' (\ss -> ss { splitStateBinders = Map.insert bid lb (splitStateBinders ss) })

remapBound :: Int -> Set Int -> Int -> Int
remapBound r bs b = maybe b (+ r) (Set.lookupIndex b bs)

splitLocScopeInner :: (Traversable f, Ord a) => Int -> Set Int -> LocScope (WithTracked a l) n f a -> State (SplitState l n f a) (SplitResult l n f a)
splitLocScopeInner r bs ls =
  case ls of
    LocScopeBound (WithTracked _ l) b ->
      let b' = remapBound r bs b
      in pure (WithTracked (mkTrackedBound b') (LocScopeBound l b'))
    LocScopeFree (WithTracked _ l) a ->
      pure (WithTracked (mkTrackedFree a) (LocScopeFree l a))
    LocScopeEmbed (WithTracked _ l) fe -> do
      fx <- traverse (splitLocScopeInner r bs) fe
      let WithTracked t fe' = sequence fx
      pure (WithTracked t (LocScopeEmbed l (SplitFunctorBase fe')))
    LocScopeBinder (WithTracked (Tracked fv bv) l) x n e -> do
      bid <- allocBinderId
      let bs' = Set.mapMonotonic (+ r) bv
      WithTracked _ e' <- splitLocScopeInner x bs' e
      let lb = SplitBinder (Set.size bs') fv (BinderScope x n e')
      insertBinder bid lb
      let ss = Seq.fromList (Set.toList bv)
      pure (WithTracked (Tracked Set.empty bv) (LocScopeEmbed l (SplitFunctorClosure bid ss)))

-- TODO Take predicate on `n` to determine which binders need to be converted.
splitLocScope :: (Traversable f, Ord a) => LocScope (WithTracked a l) n f a -> State (SplitState l n f a) (SplitResult l n f a)
splitLocScope = splitLocScopeInner 0 Set.empty
