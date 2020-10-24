module Test.Blanks.SplitTest
  ( testSplit
  ) where

-- import Blanks (BinderId (..), BinderScope (..), pattern NameOnly, pattern ScopeBinder, pattern ScopeBound,
--                pattern ScopeEmbed, pattern ScopeFree, SplitVar (..), initSplitState, locScopeForget, splitLocScope,
--                splitStateBinders)
-- import Control.Monad.State (runState)
-- -- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
-- import Test.Blanks.Exp (Exp (..), Ident (..), expToNameless, runCExpParser)
import Blanks
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Test.Blanks.SimpleScope
import Test.Blanks.SplitScope
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

data SplitCase = SplitCase
  { splitCaseName :: !String
  , splitCaseScopeIn :: !SimpleScope
  , splitCaseTrackedOut :: !(Tracked Char)
  , splitCaseScopeOut :: !SplitScope
  , splitCaseBinders :: !(Map BinderId SplitOuterBinder)
  } deriving stock (Eq, Show)

xvar, xbound, xfree, xfree2, xid, xwonky, xconst, xflip, xvar2, xwonky2, xpair, xwonky3 :: SplitScope
xvar = varSplit 'x'
xbound = boundSplit 0
xfree = abstSplit 'y' (varSplit 'x')
xfree2 = abstSplit 'z' (abstSplit 'y' (varSplit 'x'))
xid = abstSplit 'x' (varSplit 'x')
xwonky = abstSplit 'x' (boundSplit 0)
xconst = abstSplit 'x' (abstSplit 'y' (varSplit 'x'))
xflip = abstSplit 'x' (abstSplit 'y' (varSplit 'y'))
xvar2 = varSplit 'e'
xwonky2 = abstSplit 'x' xvar2
xpair = embedSplit xvar xbound
xwonky3 = abstSplit 'x' (boundSplit 3)

splitCases :: [SplitCase]
splitCases =
  [ SplitCase "var" svar (mkTrackedFree 'x') xvar Map.empty
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
  ]

runSplitCase :: SplitCase -> IO ()
runSplitCase (SplitCase _ scopeIn trackedOut scopeOut binders) = do
  let (actualTrackedOut, actualScopeOut, actualBinders) = simpleSplit scopeIn
  actualTrackedOut @?= trackedOut
  actualScopeOut @?= scopeOut
  actualBinders @?= binders

testSplitCase :: SplitCase -> TestTree
testSplitCase c = testCase (splitCaseName c) (runSplitCase c)

testSplit :: TestTree
testSplit = testGroup "Split" (fmap testSplitCase splitCases)
  -- let xIdent = Ident "x"
  --     yIdent = Ident "y"
  --     yExp = ScopeFree yIdent
  --     intExp = ScopeEmbed (ExpInt 42)
  --     addExp = ScopeEmbed (ExpAdd (ScopeBound 0) yExp)
  --     lamExp = ScopeBinder 1 (NameOnly xIdent) addExp
  --     input = "((lambda (x) (+ x y)) 42)"
  --     expectedExp = ScopeEmbed (ExpApp lamExp intExp)
  --     bid = BinderId 0
  --     expectedSplitExp = ScopeEmbed (ExpApp (ScopeFree (SplitVarBound bid)) (fmap SplitVarFree intExp))
  --     expectedSplitMap = Map.singleton bid (BinderScope 1 (NameOnly xIdent) (fmap SplitVarFree addExp))
  -- named <- runCExpParser input
  -- let nameless = expToNameless named
  -- let actualExp = locScopeForget nameless
  -- actualExp @?= expectedExp
  -- let (split, ss) = runState (splitLocScope nameless) initSplitState
  --     actualSplitExp = locScopeForget split
  --     actualSplitMap = fmap (fmap locScopeForget) (splitStateBinders ss)
  -- actualSplitExp @?= expectedSplitExp
  -- actualSplitMap @?= expectedSplitMap
