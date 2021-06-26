module Test.Blanks.SplitScope where

import Blanks (BinderId, BinderScope, LocScope, SplitBinder (..), SplitFunctor (..), SplitState, State, Tracked,
               WithTracked (..), locScopeAbstract1, locScopeInnerBinder1, locScopeLocation, pattern LocScopeBound,
               pattern LocScopeEmbed, runColocated, scopeAnno, splitLocScope, trackScope)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Blanks.SimpleScope (SimpleFunctor (..), SimpleInfo (..), SimpleScope)

type SimpleSplitState = State (SplitState () SimpleInfo SimpleFunctor Char)
type SplitScope = LocScope () SimpleInfo (SplitFunctor SimpleFunctor) Char
type SplitInnerBinder = BinderScope SimpleInfo SplitScope
type SplitOuterBinder = SplitBinder () SimpleInfo SimpleFunctor Char
type SplitScopeResult = WithTracked Char SplitScope

lamSplit :: Char -> SplitScope -> SplitScope
lamSplit a = flip runColocated () . locScopeAbstract1 (SimpleInfoLam a) a

innerLamBinderSplit :: Char -> SplitScope -> SplitInnerBinder
innerLamBinderSplit a = locScopeInnerBinder1 (SimpleInfoLam a) a

boundSplit :: Int -> SplitScope
boundSplit = LocScopeBound ()

varSplit :: Char -> SplitScope
varSplit = pure

baseSplit :: Char -> SplitScope
baseSplit = LocScopeEmbed () . SplitFunctorBase . SimpleFunctorBase

freeVarsSplit :: SplitScope -> Set Char
freeVarsSplit = foldMap Set.singleton

trackedSplit :: SplitScope -> Tracked Char
trackedSplit = withTrackedState . locScopeLocation . trackScope

appSplit :: SplitScope -> SplitScope -> SplitScope
appSplit x y = LocScopeEmbed () (SplitFunctorBase (SimpleFunctorApp x y))

closureSplit :: BinderId -> [Int] -> SplitScope
closureSplit bid vars = LocScopeEmbed () (SplitFunctorClosure bid (Seq.fromList vars))

letSplit :: Char -> SplitScope -> SplitScope -> SplitScope
letSplit a x y =
  let y' = runColocated (locScopeAbstract1 (SimpleInfoLet a) a y) ()
  in LocScopeEmbed () (SplitFunctorBase (SimpleFunctorLet x y'))

outerLamBinderSplit :: Int -> [Char] -> SplitInnerBinder -> SplitOuterBinder
outerLamBinderSplit a = SplitBinder a . Set.fromList

shouldSplitSimple :: SimpleInfo -> Bool
shouldSplitSimple i =
  case i of
    SimpleInfoLam _ -> True
    SimpleInfoLet _ -> False

simpleSplit :: SimpleScope -> SimpleSplitState SplitScopeResult
simpleSplit s = splitLocScope shouldSplitSimple (trackScope (scopeAnno () s))
