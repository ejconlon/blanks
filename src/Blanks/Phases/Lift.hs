{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Utilities to lambda-lift expressions.
module Blanks.Phases.Lift
  ( BinderId (..)
  , LiftFunctor (..)
  , LiftBinder (..)
  , LiftResult
  , LiftState (..)
  , emptyLiftState
  , liftLocScope
  ) where

import Blanks.Core (BinderScope (..))
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

data LiftFunctor f a =
    LiftFunctorBase !(f a)
  | LiftFunctorClosure !BinderId !(Seq Int)
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

data LiftBinder l n f a = LiftBinder
  { liftBinderClosureArity :: !Int
  , liftBinderFree :: !(Set a)
  , liftBinderScope :: !(BinderScope n (LocScope l n (LiftFunctor f) a))
  } deriving stock (Generic)

instance (Eq l, Eq n, Eq a, Eq (f (LocScope l n (LiftFunctor f) a))) => Eq (LiftBinder l n f a) where
  LiftBinder a1 f1 s1 == LiftBinder a2 f2 s2 = a1 == a2 && f1 == f2 && s1 == s2

instance (Show l, Show n, Show a, Show (f (LocScope l n (LiftFunctor f) a))) => Show (LiftBinder l n f a) where
  showsPrec d (LiftBinder a f s) =
    showString "LiftBinder " .
    showParen True (showsPrec (d+1) a) .
    showChar ' ' .
    showParen True (showsPrec (d+1) f) .
    showChar ' ' .
    showParen True (showsPrec (d+1) s)

instance (NFData l, NFData n, NFData a, NFData (f (LocScope l n (LiftFunctor f) a))) => NFData (LiftBinder l n f a) where
  rnf (LiftBinder a f s) = seq (rnf a) (seq (rnf f) (rnf s))

type LiftResult l n f a = WithTracked a (LocScope l n (LiftFunctor f) a)

data LiftState l n f a = LiftState
  { liftStateNextId :: !BinderId
  , liftStateBinders :: !(Map BinderId (LiftBinder l n f a))
  } deriving stock (Generic)

instance (Eq l, Eq n, Eq a, Eq (f (LocScope l n (LiftFunctor f) a))) => Eq (LiftState l n f a) where
  LiftState n1 m1 == LiftState n2 m2 = n1 == n2 && m1 == m2

instance (Show l, Show n, Show a, Show (f (LocScope l n (LiftFunctor f) a))) => Show (LiftState l n f a) where
  showsPrec d (LiftState n m) =
    showString "LiftState " .
    showParen True (showsPrec (d+1) n) .
    showChar ' ' .
    showParen True (showsPrec (d+1) m)

instance (NFData l, NFData n, NFData a, NFData (f (LocScope l n (LiftFunctor f) a))) => NFData (LiftState l n f a) where
  rnf (LiftState n m) = seq (rnf n) (rnf m)

emptyLiftState :: LiftState l n f a
emptyLiftState = LiftState (toEnum 0) Map.empty

allocBinderId :: State (LiftState l n f a) BinderId
allocBinderId = do
  next <- gets liftStateNextId
  modify' (\ss -> ss { liftStateNextId = succ next })
  pure next

insertBinder :: BinderId -> LiftBinder l n f a -> State (LiftState l n f a) ()
insertBinder bid lb = modify' (\ss -> ss { liftStateBinders = Map.insert bid lb (liftStateBinders ss) })

remapBound :: Int -> Set Int -> Int -> Int
remapBound r bs b = maybe b (+ r) (Set.lookupIndex b bs)

liftLocScopeInner :: (Traversable f, Ord a) => Int -> Set Int -> (n -> Bool) -> LocScope (WithTracked a l) n f a -> State (LiftState l n f a) (LiftResult l n f a)
liftLocScopeInner r bs p ls =
  case ls of
    LocScopeBound (WithTracked _ l) b ->
      let b' = remapBound r bs b
      in pure (WithTracked (mkTrackedBound b') (LocScopeBound l b'))
    LocScopeFree (WithTracked _ l) a ->
      pure (WithTracked (mkTrackedFree a) (LocScopeFree l a))
    LocScopeEmbed (WithTracked _ l) fe -> do
      fx <- traverse (liftLocScopeInner r bs p) fe
      let WithTracked t fe' = sequence fx
      pure (WithTracked t (LocScopeEmbed l (LiftFunctorBase fe')))
    LocScopeBinder (WithTracked (Tracked fv bv) l) x n e ->
      -- Some binders need to be lifted, some don't.
      if p n
        then do
          bid <- allocBinderId
          let bs' = Set.mapMonotonic (+ r) bv
          WithTracked _ e' <- liftLocScopeInner x bs' p e
          let lb = LiftBinder (Set.size bs') fv (BinderScope x n e')
          insertBinder bid lb
          let ss = Seq.fromList (Set.toList bv)
          pure (WithTracked (Tracked Set.empty bv) (LocScopeEmbed l (LiftFunctorClosure bid ss)))
        else do
          let bs' = Set.mapMonotonic (+ r) bv
          WithTracked (Tracked fv' _) e' <- liftLocScopeInner x bs' p e
          pure (WithTracked (Tracked fv' bv) (LocScopeBinder l x n e'))

liftLocScope :: (Traversable f, Ord a) => (n -> Bool) -> LocScope (WithTracked a l) n f a -> State (LiftState l n f a) (LiftResult l n f a)
liftLocScope = liftLocScopeInner 0 Set.empty
