module Test.Blanks.SplitTest
  ( testSplit
  ) where

import Blanks (BinderId, SplitState (..), Tracked, WithTracked (..), emptySplitState, mkTrackedBound, mkTrackedFree,
               runState)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Test.Blanks.SimpleScope (SimpleScope, sapp, sbound, sconst, sflip, sfree, sfree2, sid, slamLetBodyConst,
                                slamLetBodyFree, slamLetBodyId, slet, sletFree, sletFree2, sletLamBodyConst,
                                sletLamBodyFree, sletLamBodyId, sletWonky, sletWonky2, svar, swonky, swonky3)
import Test.Blanks.SplitScope (SplitOuterBinder, SplitScope, appSplit, baseSplit, boundSplit, closureSplit,
                               innerLamBinderSplit, letSplit, outerLamBinderSplit, simpleSplit, varSplit)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

data SplitCase = SplitCase
  { splitCaseName :: !String
  , splitCaseScopeIn :: !SimpleScope
  , splitCaseTrackedOut :: !(Tracked Char)
  , splitCaseScopeOut :: !SplitScope
  , splitCaseBinders :: !(Map BinderId SplitOuterBinder)
  } deriving stock (Eq, Show)

splitCases :: [SplitCase]
splitCases =
  let xvar = varSplit 'x'
      xbound = boundSplit 0
      xpair = appSplit xvar xbound
      xbase = baseSplit 'm'
  in [ SplitCase "var" svar (mkTrackedFree 'x') xvar Map.empty
    , SplitCase "bound" sbound (mkTrackedBound 0) xbound Map.empty
    , SplitCase "app" sapp (mkTrackedFree 'x' <> mkTrackedBound 0) xpair Map.empty
    , SplitCase "let" slet mempty (letSplit 'x' xbase xvar) Map.empty
    , SplitCase "letFree" sletFree (mkTrackedFree 'x') (letSplit 'y' xbase xvar) Map.empty
    , SplitCase "letFree2" sletFree2 (mkTrackedFree 'x') (letSplit 'y' xvar xbase) Map.empty
    , SplitCase "letWonky" sletWonky (mkTrackedBound 0) (letSplit 'y' xbase xbound) Map.empty
    , SplitCase "letWonky2" sletWonky2 (mkTrackedBound 0) (letSplit 'y' xbound xbase) Map.empty
    , let ib = innerLamBinderSplit 'z' (varSplit 'z')
          ob = outerLamBinderSplit 0 [] ib
      in SplitCase "letLamBodyId" sletLamBodyId mempty (letSplit 'y' xbase (closureSplit 0 [])) (Map.singleton 0 ob)
    , let ib = innerLamBinderSplit 'z' (boundSplit 0)
          ob = outerLamBinderSplit 1 [] ib
      in SplitCase "letLamBodyConst" sletLamBodyConst mempty (letSplit 'y' xbase (closureSplit 0 [0])) (Map.singleton 0 ob)
    , let ib = innerLamBinderSplit 'z' (varSplit 'x')
          ob = outerLamBinderSplit 0 ['x'] ib
      in SplitCase "letLamBodyFree" sletLamBodyFree mempty (letSplit 'y' xbase (closureSplit 0 [])) (Map.singleton 0 ob)
    , let ib = innerLamBinderSplit 'y' (letSplit 'z' xbase (varSplit 'z'))
          ob = outerLamBinderSplit 0 [] ib
      in SplitCase "lamLetBodyId" slamLetBodyId mempty (closureSplit 0 []) (Map.singleton 0 ob)
    , let ib = innerLamBinderSplit 'y' (letSplit 'z' xbase (varSplit 'y'))
          ob = outerLamBinderSplit 0 [] ib
      in SplitCase "lamLetBodyConst" slamLetBodyConst mempty (closureSplit 0 []) (Map.singleton 0 ob)
    , let ib = innerLamBinderSplit 'y' (letSplit 'z' xbase (varSplit 'x'))
          ob = outerLamBinderSplit 0 ['x'] ib
      in SplitCase "lamLetBodyFree" slamLetBodyFree mempty (closureSplit 0 []) (Map.singleton 0 ob)
    , let ib = innerLamBinderSplit 'y' (varSplit 'x')
          ob = outerLamBinderSplit 0 ['x'] ib
      in SplitCase "free" sfree mempty (closureSplit 0 []) (Map.singleton 0 ob)
    , let ibY = innerLamBinderSplit 'y' (varSplit 'x')
          obY = outerLamBinderSplit 0 ['x'] ibY
          ibZ = innerLamBinderSplit 'z' (closureSplit 1 [])
          obZ = outerLamBinderSplit 0 ['x'] ibZ
      in SplitCase "free2" sfree2 mempty (closureSplit 0 []) (Map.fromList [(0, obZ), (1, obY)])
    , let ib = innerLamBinderSplit 'x' (varSplit 'x')
          ob = outerLamBinderSplit 0 [] ib
      in SplitCase "id" sid mempty (closureSplit 0 []) (Map.singleton 0 ob)
    , let ib = innerLamBinderSplit 'x' (boundSplit 0)
          ob = outerLamBinderSplit 1 [] ib
      in SplitCase "wonky" swonky (mkTrackedBound 0) (closureSplit 0 [0]) (Map.singleton 0 ob)
    , let ibY = innerLamBinderSplit 'y' (boundSplit 0)
          obY = outerLamBinderSplit 1 [] ibY
          ibZ = innerLamBinderSplit 'x' (closureSplit 1 [0])
          obZ = outerLamBinderSplit 0 [] ibZ
      in SplitCase "const" sconst mempty (closureSplit 0 []) (Map.fromList [(0, obZ), (1, obY)])
    , let ibY = innerLamBinderSplit 'y' (varSplit 'y')
          obY = outerLamBinderSplit 0 [] ibY
          ibZ = innerLamBinderSplit 'x' (closureSplit 1 [])
          obZ = outerLamBinderSplit 0 [] ibZ
      in SplitCase "flip" sflip mempty (closureSplit 0 []) (Map.fromList [(0, obZ), (1, obY)])
    , let ib = innerLamBinderSplit 'x' (boundSplit 3)
          ob = outerLamBinderSplit 1 [] ib
      in SplitCase "wonky3" swonky3 (mkTrackedBound 3) (closureSplit 0 [3]) (Map.singleton 0 ob)
    ]

runSplitCase :: SplitCase -> IO ()
runSplitCase (SplitCase _ scopeIn trackedOut scopeOut binders) = do
  let (WithTracked actualTrackedOut actualScopeOut, SplitState _ actualBinders) = runState (simpleSplit scopeIn) emptySplitState
  actualTrackedOut @?= trackedOut
  actualScopeOut @?= scopeOut
  actualBinders @?= binders

testSplitCase :: SplitCase -> TestTree
testSplitCase c = testCase (splitCaseName c) (runSplitCase c)

testSplit :: TestTree
testSplit = testGroup "Split" (fmap testSplitCase splitCases)
