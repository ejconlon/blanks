module Test.Blanks.SplitTest
  ( testSplit
  ) where

import Blanks (BinderId (..), SplitVar (..), BinderScope (..), locScopeForget, splitLocScope, initSplitState, splitStateBinders, pattern NameOnly, pattern ScopeEmbed, pattern ScopeBinder, pattern ScopeBound, pattern ScopeFree)
import Control.Monad.State (runState)
-- import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Test.Blanks.Exp (Exp (..), Ident (..), expToNameless, runCExpParser)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit ((@?=), testCase)

testSplit :: TestTree
testSplit = testCase "Split" $ do
  let xIdent = Ident "x"
      yIdent = Ident "y"
      yExp = ScopeFree yIdent
      intExp = ScopeEmbed (ExpInt 42)
      addExp = ScopeEmbed (ExpAdd (ScopeBound 0) yExp)
      lamExp = ScopeBinder 1 (NameOnly xIdent) addExp
      input = "((lambda (x) (+ x y)) 42)"
      expectedExp = ScopeEmbed (ExpApp lamExp intExp)
      bid = BinderId 0
      expectedSplitExp = ScopeEmbed (ExpApp (ScopeFree (SplitVarBound bid)) (fmap SplitVarFree intExp))
      expectedSplitMap = Map.singleton bid (BinderScope 1 (NameOnly xIdent) (fmap SplitVarFree addExp))
  named <- runCExpParser input
  let nameless = expToNameless named
  let actualExp = locScopeForget nameless
  actualExp @?= expectedExp
  let (split, ss) = runState (splitLocScope nameless) initSplitState
      actualSplitExp = locScopeForget split
      actualSplitMap = fmap (fmap locScopeForget) (splitStateBinders ss)
  actualSplitExp @?= expectedSplitExp
  actualSplitMap @?= expectedSplitMap
