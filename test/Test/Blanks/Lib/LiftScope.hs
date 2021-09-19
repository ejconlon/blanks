module Test.Blanks.Lib.LiftScope where

import Blanks (AbstractId, Abstract (..), LiftAbstract (..), LiftFunctor (..), LiftState, LocScope, Located (..), State,
               Tracked, WithTracked, locScopeBindFree1, locScopeLocation, pattern LocScopeAbstract, pattern LocScopeBound,
               pattern LocScopeEmbed, predLiftLocScope, scopeAnno, trackScope)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Blanks.Lib.SimpleScope (SimpleFunctor (..), SimpleInfo (..), SimpleScope)

type SimpleLiftState = State (LiftState () SimpleInfo SimpleInfo SimpleFunctor Char)
type LiftScope = LocScope () SimpleInfo (LiftFunctor SimpleFunctor) Char
type LiftInnerAbstract = Abstract SimpleInfo LiftScope
type LiftOuterAbstract = LiftAbstract () SimpleInfo SimpleInfo SimpleFunctor Char
type LiftScopeResult = WithTracked Char LiftScope

lamLift :: Char -> LiftScope -> LiftScope
lamLift a = LocScopeAbstract () . innerLamAbstractLift a

innerLamAbstractLift :: Char -> LiftScope -> LiftInnerAbstract
innerLamAbstractLift a = Abstract (SimpleInfoLam (Seq.singleton a)) . locScopeBindFree1 a

boundLift :: Int -> LiftScope
boundLift = LocScopeBound ()

varLift :: Char -> LiftScope
varLift = pure

baseLift :: Char -> LiftScope
baseLift = LocScopeEmbed () . LiftFunctorBase . SimpleFunctorBase

freeVarsLift :: LiftScope -> Set Char
freeVarsLift = foldMap Set.singleton

trackedLift :: LiftScope -> Tracked Char
trackedLift = locatedLoc . locScopeLocation . trackScope

appLift :: LiftScope -> LiftScope -> LiftScope
appLift x y = LocScopeEmbed () (LiftFunctorBase (SimpleFunctorApp x y))

closureLift :: AbstractId -> [Int] -> LiftScope
closureLift bid vars = LocScopeEmbed () (LiftFunctorAbstract bid (Seq.fromList vars))

letLift :: Char -> LiftScope -> LiftScope -> LiftScope
letLift a x y =
  let y' = LocScopeAbstract () (Abstract (SimpleInfoLet a) (locScopeBindFree1 a y))
  in LocScopeEmbed () (LiftFunctorBase (SimpleFunctorLet x y'))

outerLamAbstractLift :: Int -> [Char] -> LiftInnerAbstract -> LiftOuterAbstract
outerLamAbstractLift a = LiftAbstract a . Set.fromList

shouldLiftSimple :: SimpleInfo e -> Bool
shouldLiftSimple i =
  case i of
    SimpleInfoLam _ -> True
    SimpleInfoLet _ -> False

simpleLift :: SimpleScope -> SimpleLiftState LiftScopeResult
simpleLift s = predLiftLocScope shouldLiftSimple (trackScope (scopeAnno () s))
