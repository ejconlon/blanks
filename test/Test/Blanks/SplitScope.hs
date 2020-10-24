module Test.Blanks.SplitScope where

import Blanks (BinderId, BinderScope, LocScope, pattern LocScopeBound, pattern LocScopeEmbed, NameOnly,
               pattern NameOnly, SplitBinder (..), SplitFunctor (..), SplitState (splitStateBinders), Tracked,
               WithTracked (withTrackedState), initSplitState, locScopeAbstract1, locScopeInnerBinder1,
               locScopeLocation, runColocated, scopeAnno, splitLocScope, trackScope)
import Control.Monad.State (runState)
import Data.Map.Strict (Map)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Blanks.SimpleScope (SimpleFunctor (..), SimpleScope)

type SplitScope = LocScope () (NameOnly Char) (SplitFunctor SimpleFunctor) Char
type SplitInnerBinder = BinderScope (NameOnly Char) SplitScope
type SplitOuterBinder = SplitBinder () (NameOnly Char) SimpleFunctor Char

abstSplit :: Char -> SplitScope -> SplitScope
abstSplit a = flip runColocated () . locScopeAbstract1 (NameOnly a) a

innerBinderSplit :: Char -> SplitScope -> SplitInnerBinder
innerBinderSplit a = locScopeInnerBinder1 (NameOnly a) a

boundSplit :: Int -> SplitScope
boundSplit = LocScopeBound ()

varSplit :: Char -> SplitScope
varSplit = pure

freeVarsSplit :: SplitScope -> Set Char
freeVarsSplit = foldMap Set.singleton

trackedSplit :: SplitScope -> Tracked Char
trackedSplit = withTrackedState . locScopeLocation . trackScope

embedSplit :: SplitScope -> SplitScope -> SplitScope
embedSplit x y = LocScopeEmbed () (SplitFunctorBase (SimpleFunctor x y))

closureSplit :: BinderId -> [Int] -> SplitScope
closureSplit bid vars = LocScopeEmbed () (SplitFunctorClosure bid (Seq.fromList vars))

outerBinderSplit :: Int -> [Char] -> SplitInnerBinder -> SplitOuterBinder
outerBinderSplit a = SplitBinder a . Set.fromList

simpleSplit :: SimpleScope -> (Tracked Char, SplitScope, Map BinderId SplitOuterBinder)
simpleSplit s =
  let ((t, s'), st) = runState (splitLocScope (trackScope (scopeAnno () s))) initSplitState
  in (t, s', splitStateBinders st)
