{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Utilities to lambda-lift expressions.
module Blanks.Transform.Lift
  ( LiftSelection (..)
  , LiftSelector
  , predSelector
  , BinderId (..)
  , LiftFunctor (..)
  , LiftBinder (..)
  , LiftResult
  , LiftState (..)
  , emptyLiftState
  , liftLocScope
  , predLiftLocScope
  ) where

import Blanks.Internal.Core (BinderScope (..))
import Blanks.LocScope (LocScope, pattern LocScopeBinder, pattern LocScopeBound, pattern LocScopeEmbed,
                        pattern LocScopeFree)
import Blanks.Transform.Track (Tracked (..), WithTracked (..), mkTrackedBound, mkTrackedFree)
import Control.DeepSeq (NFData (..))
import Control.Monad.State.Strict (State, gets, modify')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)

-- | Should we select this binder for lifting? (Glorified boolean.)
data LiftSelection x y =
    LiftSelectionNo !x
  | LiftSelectionYes !y
  deriving stock (Eq, Show)

-- | Function to transform and classify binder info for lifting or not.
type LiftSelector n x y = n -> LiftSelection x y

-- | Selects the binder for lifting if the predicate is satisfies.
predSelector :: (n -> Bool) -> LiftSelector n n n
predSelector p n = if p n then LiftSelectionYes n else LiftSelectionNo n

-- | An anonymous identifier for lifted binders.
newtype BinderId = BinderId { unBinderId :: Int }
  deriving stock (Eq, Show, Ord)
  deriving newtype (Enum, NFData, Num)

-- | When lifting binders, we rewrite the scope functor with this, embedding the original
-- functor and adding a constructor to refer to a lifted binder.
data LiftFunctor f a =
    LiftFunctorBase !(f a)
  | LiftFunctorBinder !BinderId !(Seq Int)  -- ^ Bound variables - see 'liftBinderClosureArity'.
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

-- | A lifted binder.
data LiftBinder l x y f a = LiftBinder
  { liftBinderClosureArity :: !Int  -- ^ Number of bound variables from enclosing scope. 'LiftFunctorBinder' binds them.
  , liftBinderFree :: !(Set a)  -- ^ Tracked free variables in the binder scope
  , liftBinderScope :: !(BinderScope y (LocScope l x (LiftFunctor f) a))  -- ^ Binder scope
  } deriving stock (Generic)

instance (Eq l, Eq x, Eq y, Eq a, Eq (f (LocScope l x (LiftFunctor f) a))) => Eq (LiftBinder l x y f a) where
  LiftBinder a1 f1 s1 == LiftBinder a2 f2 s2 = a1 == a2 && f1 == f2 && s1 == s2

instance (Show l, Show x, Show y, Show a, Show (f (LocScope l x (LiftFunctor f) a))) => Show (LiftBinder l x y f a) where
  showsPrec d (LiftBinder a f s) =
    showString "LiftBinder " .
    showParen True (showsPrec (d+1) a) .
    showChar ' ' .
    showParen True (showsPrec (d+1) f) .
    showChar ' ' .
    showParen True (showsPrec (d+1) s)

instance (NFData l, NFData x, NFData y, NFData a, NFData (f (LocScope l x (LiftFunctor f) a))) => NFData (LiftBinder l x y f a) where
  rnf (LiftBinder a f s) = seq (rnf a) (seq (rnf f) (rnf s))

-- | Result for 'liftLocScope'. Contains tracked free variables and the scope tree.
type LiftResult l n f a = WithTracked a (LocScope l n (LiftFunctor f) a)

-- | State for 'liftLocScope'. Contains next binder ID and map of all lifted binders.
data LiftState l x y f a = LiftState
  { liftStateNextId :: !BinderId
  , liftStateBinders :: !(Map BinderId (LiftBinder l x y f a))
  } deriving stock (Generic)

instance (Eq l, Eq x, Eq y, Eq a, Eq (f (LocScope l x (LiftFunctor f) a))) => Eq (LiftState l x y f a) where
  LiftState n1 m1 == LiftState n2 m2 = n1 == n2 && m1 == m2

instance (Show l, Show x, Show y, Show a, Show (f (LocScope l x (LiftFunctor f) a))) => Show (LiftState l x y f a) where
  showsPrec d (LiftState n m) =
    showString "LiftState " .
    showParen True (showsPrec (d+1) n) .
    showChar ' ' .
    showParen True (showsPrec (d+1) m)

instance (NFData l, NFData x, NFData y, NFData a, NFData (f (LocScope l x (LiftFunctor f) a))) => NFData (LiftState l x y f a) where
  rnf (LiftState n m) = seq (rnf n) (rnf m)

-- | Empty lift state
emptyLiftState :: LiftState l x y f a
emptyLiftState = LiftState (toEnum 0) Map.empty

allocBinderId :: State (LiftState l x y f a) BinderId
allocBinderId = do
  next <- gets liftStateNextId
  modify' (\ss -> ss { liftStateNextId = succ next })
  pure next

insertBinder :: BinderId -> LiftBinder l x y f a -> State (LiftState l x y f a) ()
insertBinder bid lb = modify' (\ss -> ss { liftStateBinders = Map.insert bid lb (liftStateBinders ss) })

remapBound :: Int -> Set Int -> Int -> Int
remapBound r bs b = maybe b (+ r) (Set.lookupIndex b bs)

liftLocScopeInner :: (Traversable f, Ord a) => Int -> Set Int -> LiftSelector n x y -> LocScope (WithTracked a l) n f a -> State (LiftState l x y f a) (LiftResult l x f a)
liftLocScopeInner r bs sel ls =
  case ls of
    LocScopeBound (WithTracked _ l) b ->
      let b' = remapBound r bs b
      in pure (WithTracked (mkTrackedBound b') (LocScopeBound l b'))
    LocScopeFree (WithTracked _ l) a ->
      pure (WithTracked (mkTrackedFree a) (LocScopeFree l a))
    LocScopeEmbed (WithTracked _ l) fe -> do
      fx <- traverse (liftLocScopeInner r bs sel) fe
      let WithTracked t fe' = sequence fx
      pure (WithTracked t (LocScopeEmbed l (LiftFunctorBase fe')))
    LocScopeBinder (WithTracked (Tracked fv bv) l) a n e ->
      -- Some binders need to be lifted, some don't.
      case sel n of
        LiftSelectionYes y -> do
          bid <- allocBinderId
          let bs' = Set.mapMonotonic (+ r) bv
          WithTracked _ e' <- liftLocScopeInner a bs' sel e
          let lb = LiftBinder (Set.size bs') fv (BinderScope a y e')
          insertBinder bid lb
          let ss = Seq.fromList (Set.toList bv)
          pure (WithTracked (Tracked Set.empty bv) (LocScopeEmbed l (LiftFunctorBinder bid ss)))
        LiftSelectionNo x -> do
          let bs' = Set.mapMonotonic (+ r) bv
          WithTracked (Tracked fv' _) e' <- liftLocScopeInner a bs' sel e
          pure (WithTracked (Tracked fv' bv) (LocScopeBinder l a x e'))

-- | Lifts selected binders. To maintain binder ID state across invocations, compose in the 'State' monad and project out once at the end
-- with 'runState'. (Note that you will have to accurately annotate with tracked variables using 'trackScope' before using this.)
liftLocScope :: (Traversable f, Ord a) => LiftSelector n x y -> LocScope (WithTracked a l) n f a -> State (LiftState l x y f a) (LiftResult l x f a)
liftLocScope = liftLocScopeInner 0 Set.empty

-- | Lifts binders without transforming them. Selects according to the given predicate (see 'predSelector').
predLiftLocScope :: (Traversable f, Ord a) => (n -> Bool) -> LocScope (WithTracked a l) n f a -> State (LiftState l n n f a) (LiftResult l n f a)
predLiftLocScope = liftLocScope . predSelector
