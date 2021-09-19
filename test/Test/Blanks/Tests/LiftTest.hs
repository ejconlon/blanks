module Test.Blanks.Tests.LiftTest
  ( testLift
  ) where

import Blanks (AbstractId, LiftState (..), Located (..), Tracked, emptyLiftState, mkTrackedBound, mkTrackedFree, runState)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Test.Blanks.Lib.LiftScope (LiftOuterAbstract, LiftScope, appLift, baseLift, boundLift, closureLift,
                                  innerLamAbstractLift, letLift, outerLamAbstractLift, simpleLift, varLift)
import Test.Blanks.Lib.SimpleScope (SimpleScope, sapp, sbound, sconst, sflip, sfree, sfree2, sid, slamLetArgFree,
                                    slamLetArgOuter, slamLetBodyConst, slamLetBodyFree, slamLetBodyId, slet, sletFree,
                                    sletFree2, sletLamArgFree, sletLamArgInner, sletLamBodyConst, sletLamBodyFree,
                                    sletLamBodyId, sletWonky, sletWonky2, svar, swonky, swonky3)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

data LiftCase = LiftCase
  { liftCaseName :: !String
  , liftCaseScopeIn :: !SimpleScope
  , liftCaseTrackedOut :: !(Tracked Char)
  , liftCaseScopeOut :: !LiftScope
  , liftCaseAbstracts :: !(Map AbstractId LiftOuterAbstract)
  } deriving stock (Eq, Show)

liftCases :: [LiftCase]
liftCases =
  let xvar = varLift 'x'
      xbound = boundLift 0
      xpair = appLift xvar xbound
      xbase = baseLift 'm'
  in [ LiftCase "var" svar (mkTrackedFree 'x') xvar Map.empty
    , LiftCase "bound" sbound (mkTrackedBound 0) xbound Map.empty
    , LiftCase "app" sapp (mkTrackedFree 'x' <> mkTrackedBound 0) xpair Map.empty
    , LiftCase "let" slet mempty (letLift 'x' xbase xvar) Map.empty
    , LiftCase "letFree" sletFree (mkTrackedFree 'x') (letLift 'y' xbase xvar) Map.empty
    , LiftCase "letFree2" sletFree2 (mkTrackedFree 'x') (letLift 'y' xvar xbase) Map.empty
    , LiftCase "letWonky" sletWonky (mkTrackedBound 0) (letLift 'y' xbase xbound) Map.empty
    , LiftCase "letWonky2" sletWonky2 (mkTrackedBound 0) (letLift 'y' xbound xbase) Map.empty
    , let ib = innerLamAbstractLift 'z' (varLift 'z')
          ob = outerLamAbstractLift 0 [] ib
      in LiftCase "letLamBodyId" sletLamBodyId mempty (letLift 'y' xbase (closureLift 0 [])) (Map.singleton 0 ob)
    , let ib = innerLamAbstractLift 'z' (boundLift 0)
          ob = outerLamAbstractLift 1 [] ib
      in LiftCase "letLamBodyConst" sletLamBodyConst mempty (letLift 'y' xbase (closureLift 0 [0])) (Map.singleton 0 ob)
    , let ib = innerLamAbstractLift 'z' (varLift 'x')
          ob = outerLamAbstractLift 0 ['x'] ib
      in LiftCase "letLamBodyFree" sletLamBodyFree mempty (letLift 'y' xbase (closureLift 0 [])) (Map.singleton 0 ob)
    , let ib = innerLamAbstractLift 'y' (letLift 'z' xbase (varLift 'z'))
          ob = outerLamAbstractLift 0 [] ib
      in LiftCase "lamLetBodyId" slamLetBodyId mempty (closureLift 0 []) (Map.singleton 0 ob)
    , let ib = innerLamAbstractLift 'y' (letLift 'z' xbase (varLift 'y'))
          ob = outerLamAbstractLift 0 [] ib
      in LiftCase "lamLetBodyConst" slamLetBodyConst mempty (closureLift 0 []) (Map.singleton 0 ob)
    , let ib = innerLamAbstractLift 'y' (letLift 'z' xbase (varLift 'x'))
          ob = outerLamAbstractLift 0 ['x'] ib
      in LiftCase "lamLetBodyFree" slamLetBodyFree mempty (closureLift 0 []) (Map.singleton 0 ob)
    , let ib = innerLamAbstractLift 'z' (varLift 'z')
          ob = outerLamAbstractLift 0 [] ib
      in LiftCase "letLamArgInner" sletLamArgInner mempty (letLift 'y' (closureLift 0 []) xbase) (Map.singleton 0 ob)
    , let ib = innerLamAbstractLift 'z' (varLift 'x')
          ob = outerLamAbstractLift 0 ['x'] ib
      in LiftCase "letLamArgFree" sletLamArgFree mempty (letLift 'y' (closureLift 0 []) xbase) (Map.singleton 0 ob)
    , let ib = innerLamAbstractLift 'y' (letLift 'z' (varLift 'y') xbase)
          ob = outerLamAbstractLift 0 [] ib
      in LiftCase "lamLetArgOuter" slamLetArgOuter mempty (closureLift 0 []) (Map.singleton 0 ob)
    , let ib = innerLamAbstractLift 'y' (letLift 'z' (varLift 'x') xbase)
          ob = outerLamAbstractLift 0 ['x'] ib
      in LiftCase "lamLetArgFree" slamLetArgFree mempty (closureLift 0 []) (Map.singleton 0 ob)
    , let ib = innerLamAbstractLift 'y' (varLift 'x')
          ob = outerLamAbstractLift 0 ['x'] ib
      in LiftCase "free" sfree mempty (closureLift 0 []) (Map.singleton 0 ob)
    , let ibY = innerLamAbstractLift 'y' (varLift 'x')
          obY = outerLamAbstractLift 0 ['x'] ibY
          ibZ = innerLamAbstractLift 'z' (closureLift 1 [])
          obZ = outerLamAbstractLift 0 ['x'] ibZ
      in LiftCase "free2" sfree2 mempty (closureLift 0 []) (Map.fromList [(0, obZ), (1, obY)])
    , let ib = innerLamAbstractLift 'x' (varLift 'x')
          ob = outerLamAbstractLift 0 [] ib
      in LiftCase "id" sid mempty (closureLift 0 []) (Map.singleton 0 ob)
    , let ib = innerLamAbstractLift 'x' (boundLift 0)
          ob = outerLamAbstractLift 1 [] ib
      in LiftCase "wonky" swonky (mkTrackedBound 0) (closureLift 0 [0]) (Map.singleton 0 ob)
    , let ibY = innerLamAbstractLift 'y' (boundLift 0)
          obY = outerLamAbstractLift 1 [] ibY
          ibZ = innerLamAbstractLift 'x' (closureLift 1 [0])
          obZ = outerLamAbstractLift 0 [] ibZ
      in LiftCase "const" sconst mempty (closureLift 0 []) (Map.fromList [(0, obZ), (1, obY)])
    , let ibY = innerLamAbstractLift 'y' (varLift 'y')
          obY = outerLamAbstractLift 0 [] ibY
          ibZ = innerLamAbstractLift 'x' (closureLift 1 [])
          obZ = outerLamAbstractLift 0 [] ibZ
      in LiftCase "flip" sflip mempty (closureLift 0 []) (Map.fromList [(0, obZ), (1, obY)])
    , let ib = innerLamAbstractLift 'x' (boundLift 3)
          ob = outerLamAbstractLift 1 [] ib
      in LiftCase "wonky3" swonky3 (mkTrackedBound 3) (closureLift 0 [3]) (Map.singleton 0 ob)
    ]

runLiftCase :: LiftCase -> IO ()
runLiftCase (LiftCase _ scopeIn trackedOut scopeOut binders) = do
  let (Located actualTrackedOut actualScopeOut, LiftState _ actualAbstracts) = runState (simpleLift scopeIn) emptyLiftState
  actualTrackedOut @?= trackedOut
  actualScopeOut @?= scopeOut
  actualAbstracts @?= binders

testLiftCase :: LiftCase -> TestTree
testLiftCase c = testCase (liftCaseName c) (runLiftCase c)

testLift :: TestTree
testLift = testGroup "Lift" (fmap testLiftCase liftCases)
