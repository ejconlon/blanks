module Test.Blanks.SplitTest
  ( testSplit
  ) where

import Blanks (BinderId, SplitResult (..), Tracked, mkTrackedBound, mkTrackedFree)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Test.Blanks.SimpleScope (SimpleScope, sbound, sconst, sflip, sfree, sfree2, sid, spair, svar, swonky, swonky3)
import Test.Blanks.SplitScope (SplitOuterBinder, SplitScope, boundSplit, closureSplit, embedSplit, innerBinderSplit,
                               outerBinderSplit, simpleSplit, varSplit)
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
      xpair = embedSplit xvar xbound
  in [ SplitCase "var" svar (mkTrackedFree 'x') xvar Map.empty
    , SplitCase "bound" sbound (mkTrackedBound 0) xbound Map.empty
    , SplitCase "pair" spair (mkTrackedFree 'x' <> mkTrackedBound 0) xpair Map.empty
    , let ib = innerBinderSplit 'y' (varSplit 'x')
          ob = outerBinderSplit 0 ['x'] ib
      in SplitCase "free" sfree mempty (closureSplit 0 []) (Map.singleton 0 ob)
    , let ibY = innerBinderSplit 'y' (varSplit 'x')
          obY = outerBinderSplit 0 ['x'] ibY
          ibZ = innerBinderSplit 'z' (closureSplit 1 [])
          obZ = outerBinderSplit 0 ['x'] ibZ
      in SplitCase "free2" sfree2 mempty (closureSplit 0 []) (Map.fromList [(0, obZ), (1, obY)])
    , let ib = innerBinderSplit 'x' (varSplit 'x')
          ob = outerBinderSplit 0 [] ib
      in SplitCase "id" sid mempty (closureSplit 0 []) (Map.singleton 0 ob)
    , let ib = innerBinderSplit 'x' (boundSplit 0)
          ob = outerBinderSplit 1 [] ib
      in SplitCase "wonky" swonky (mkTrackedBound 0) (closureSplit 0 [0]) (Map.singleton 0 ob)
    , let ibY = innerBinderSplit 'y' (boundSplit 0)
          obY = outerBinderSplit 1 [] ibY
          ibZ = innerBinderSplit 'x' (closureSplit 1 [0])
          obZ = outerBinderSplit 0 [] ibZ
      in SplitCase "const" sconst mempty (closureSplit 0 []) (Map.fromList [(0, obZ), (1, obY)])
    , let ibY = innerBinderSplit 'y' (varSplit 'y')
          obY = outerBinderSplit 0 [] ibY
          ibZ = innerBinderSplit 'x' (closureSplit 1 [])
          obZ = outerBinderSplit 0 [] ibZ
      in SplitCase "flip" sflip mempty (closureSplit 0 []) (Map.fromList [(0, obZ), (1, obY)])
    , let ib = innerBinderSplit 'x' (boundSplit 3)
          ob = outerBinderSplit 1 [] ib
      in SplitCase "wonky3" swonky3 (mkTrackedBound 3) (closureSplit 0 [3]) (Map.singleton 0 ob)
    ]

runSplitCase :: SplitCase -> IO ()
runSplitCase (SplitCase _ scopeIn trackedOut scopeOut binders) = do
  let SplitResult actualTrackedOut actualScopeOut actualBinders = simpleSplit scopeIn
  actualTrackedOut @?= trackedOut
  actualScopeOut @?= scopeOut
  actualBinders @?= binders

testSplitCase :: SplitCase -> TestTree
testSplitCase c = testCase (splitCaseName c) (runSplitCase c)

testSplit :: TestTree
testSplit = testGroup "Split" (fmap testSplitCase splitCases)
