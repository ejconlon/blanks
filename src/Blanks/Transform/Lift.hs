{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Utilities to lambda-lift expressions.
module Blanks.Transform.Lift
  ( LiftSelection (..)
  , LiftSelector
  , predSelector
  , AbstractId (..)
  , LiftFunctor (..)
  , LiftAbstract (..)
  , LiftResult
  , LiftState (..)
  , emptyLiftState
  , liftLocScope
  , predLiftLocScope
  ) where

import Blanks.Internal.Abstract (Abstract (..), IsAbstractInfo (..))
import Blanks.LocScope (LocScope, pattern LocScopeAbstract, pattern LocScopeBound, pattern LocScopeEmbed,
                        pattern LocScopeFree)
import Blanks.Transform.Track (Tracked (..), WithTracked, mkTrackedBound, mkTrackedFree)
import Blanks.Util.Located (Located (..))
import Control.DeepSeq (NFData (..))
import Control.Monad.State.Strict (State, gets, modify')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)

-- | Should we select this abstraction for lifting?
data LiftSelection c d a =
    LiftSelectionNo !(c a)
  | LiftSelectionYes !(d a)
  deriving stock (Eq, Show)

-- | Function to transform and classify abstraction info for lifting or not.
type LiftSelector n c d = forall x. n x -> LiftSelection c d x

-- | Selects the abstract for lifting if the predicate is satisfies.
predSelector :: (forall x. n x -> Bool) -> LiftSelector n n n
predSelector p na = if p na then LiftSelectionYes na else LiftSelectionNo na

-- | An anonymous identifier for lifted abstractions.
newtype AbstractId = AbstractId { unAbstractId :: Int }
  deriving stock (Eq, Show, Ord)
  deriving newtype (Enum, NFData, Num)

-- | When lifting abstracts, we rewrite the scope functor with this, embedding the original
-- functor and adding a constructor to refer to a lifted abstraction.
data LiftFunctor f a =
    LiftFunctorBase !(f a)
  | LiftFunctorAbstract !AbstractId !(Seq Int)  -- ^ Bound variables - see 'liftAbstractClosureArity'.
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

-- | A lifted abstraction.
data LiftAbstract l c d f a = LiftAbstract
  { liftAbstractClosureArity :: !Int  -- ^ Number of bound variables from enclosing scope. 'LiftFunctorAbstract' binds them.
  , liftAbstractFree :: !(Set a)  -- ^ Tracked free variables in the abstraction
  , liftAbstractScope :: !(Abstract d (LocScope l c (LiftFunctor f) a))  -- ^ Abstraction
  } deriving stock (Generic)

instance
  ( Eq l, Eq a
  , Eq (c (LocScope l c (LiftFunctor f) a))
  , Eq (d (LocScope l c (LiftFunctor f) a))
  , Eq (f (LocScope l c (LiftFunctor f) a))
  ) => Eq (LiftAbstract l c d f a) where
  LiftAbstract a1 f1 s1 == LiftAbstract a2 f2 s2 = a1 == a2 && f1 == f2 && s1 == s2

instance
  ( Show l, Show a
  , Show (c (LocScope l c (LiftFunctor f) a))
  , Show (d (LocScope l c (LiftFunctor f) a))
  , Show (f (LocScope l c (LiftFunctor f) a))
  ) => Show (LiftAbstract l c d f a) where
  showsPrec d (LiftAbstract a f s) =
    showString "LiftAbstract " .
    showParen True (showsPrec (d+1) a) .
    showChar ' ' .
    showParen True (showsPrec (d+1) f) .
    showChar ' ' .
    showParen True (showsPrec (d+1) s)

instance
  ( NFData l, NFData a
  , NFData (c (LocScope l c (LiftFunctor f) a))
  , NFData (d (LocScope l c (LiftFunctor f) a))
  , NFData (f (LocScope l c (LiftFunctor f) a))
  ) => NFData (LiftAbstract l c d f a) where
  rnf (LiftAbstract a f s) = seq (rnf a) (seq (rnf f) (rnf s))

-- | Result for 'liftLocScope'. Contains tracked free variables and the scope tree.
type LiftResult l n f a = WithTracked a (LocScope l n (LiftFunctor f) a)

seqLiftResult :: (Traversable d, Ord a) => d (LiftResult l c f a) -> WithTracked a (d (LocScope l c (LiftFunctor f) a))
seqLiftResult = sequence

-- | State for 'liftLocScope'. Contains next abstract id and map of all lifted abstractions.
data LiftState l x y f a = LiftState
  { liftStateNextId :: !AbstractId
  , liftStateAbstracts :: !(Map AbstractId (LiftAbstract l x y f a))
  } deriving stock (Generic)

instance
  ( Eq l, Eq a
  , Eq (c (LocScope l c (LiftFunctor f) a))
  , Eq (d (LocScope l c (LiftFunctor f) a))
  , Eq (f (LocScope l c (LiftFunctor f) a))
  ) => Eq (LiftState l c d f a) where
  LiftState n1 m1 == LiftState n2 m2 = n1 == n2 && m1 == m2

instance
  ( Show l, Show a
  , Show (c (LocScope l c (LiftFunctor f) a))
  , Show (d (LocScope l c (LiftFunctor f) a))
  , Show (f (LocScope l c (LiftFunctor f) a))
  ) => Show (LiftState l c d f a) where
  showsPrec d (LiftState n m) =
    showString "LiftState " .
    showParen True (showsPrec (d+1) n) .
    showChar ' ' .
    showParen True (showsPrec (d+1) m)

instance
  ( NFData l, NFData a
  , NFData (c (LocScope l c (LiftFunctor f) a))
  , NFData (d (LocScope l c (LiftFunctor f) a))
  , NFData (f (LocScope l c (LiftFunctor f) a))
  ) => NFData (LiftState l c d f a) where
  rnf (LiftState n m) = seq (rnf n) (rnf m)

-- | Empty lift state
emptyLiftState :: LiftState l x y f a
emptyLiftState = LiftState (toEnum 0) Map.empty

allocAbstractId :: State (LiftState l x y f a) AbstractId
allocAbstractId = do
  next <- gets liftStateNextId
  modify' (\ss -> ss { liftStateNextId = succ next })
  pure next

insertAbstract :: AbstractId -> LiftAbstract l x y f a -> State (LiftState l x y f a) ()
insertAbstract bid lb = modify' (\ss -> ss { liftStateAbstracts = Map.insert bid lb (liftStateAbstracts ss) })

remapBound :: Int -> Set Int -> Int -> Int
remapBound r bs b = maybe b (+ r) (Set.lookupIndex b bs)

liftLocScopeInner ::
  (Traversable c, Traversable d, IsAbstractInfo n, Traversable f, Ord a)
  => Int -> Set Int -> LiftSelector n c d -> LocScope (WithTracked a l) n f a -> State (LiftState l c d f a) (LiftResult l c f a)
liftLocScopeInner r bs sel ls =
  case ls of
    LocScopeBound (Located _ l) b ->
      let b' = remapBound r bs b
      in pure (Located (mkTrackedBound b') (LocScopeBound l b'))
    LocScopeFree (Located _ l) a ->
      pure (Located (mkTrackedFree a) (LocScopeFree l a))
    LocScopeEmbed (Located _ l) fe -> do
      fx <- traverse (liftLocScopeInner r bs sel) fe
      let Located t fe' = sequence fx
      pure (Located t (LocScopeEmbed l (LiftFunctorBase fe')))
    LocScopeAbstract (Located (Tracked fv bv) l) (Abstract n e) ->
      -- Some abstractions need to be lifted, some don't.
      let a = abstractInfoArity n
      in case sel n of
        LiftSelectionYes y -> do
          -- First process recursive parts of the info function
          y' <- traverse (liftLocScopeInner r bs sel) y
          let Located _ y'' = seqLiftResult y'
          -- Now allocate an id for this abstraction and process the body
          bid <- allocAbstractId
          let bs' = Set.mapMonotonic (+ r) bv
          Located _ e' <- liftLocScopeInner a bs' sel e
          let lb = LiftAbstract (Set.size bs') fv (Abstract y'' e')
          insertAbstract bid lb
          let ss = Seq.fromList (Set.toList bv)
          pure (Located (Tracked Set.empty bv) (LocScopeEmbed l (LiftFunctorAbstract bid ss)))
        LiftSelectionNo x -> do
          -- First process recursive parts of the info function
          x' <- traverse (liftLocScopeInner r bs sel) x
          let Located _ x'' = seqLiftResult x'
          -- Now just process the recursive parts of the body
          let bs' = Set.mapMonotonic (+ r) bv
          Located (Tracked fv' _) e' <- liftLocScopeInner a bs' sel e
          pure (Located (Tracked fv' bv) (LocScopeAbstract l (Abstract x'' e')))

-- | Lifts selected abstractions. To maintain abstract id state across invocations, compose in the 'State' monad and project out once at the end
-- with 'runState'. (Note that you will have to accurately annotate with tracked variables using 'trackScope' before using this.)
liftLocScope ::
  (IsAbstractInfo n, Traversable c, Traversable d, Traversable f, Ord a)
  => LiftSelector n c d -> LocScope (WithTracked a l) n f a -> State (LiftState l c d f a) (LiftResult l c f a)
liftLocScope = liftLocScopeInner 0 Set.empty

-- | Lifts abstractions without transforming them. Selects according to the given predicate (see 'predSelector').
predLiftLocScope ::
  (IsAbstractInfo n, Traversable f, Ord a)
  => (forall x. n x -> Bool) -> LocScope (WithTracked a l) n f a -> State (LiftState l n n f a) (LiftResult l n f a)
predLiftLocScope p = liftLocScope (predSelector p)
