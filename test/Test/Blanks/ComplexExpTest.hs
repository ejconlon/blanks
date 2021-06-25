module Test.Blanks.ComplexExpTest
  ( testComplexExp
  ) where

import Blanks.Global (globalScope, predClassifier)
import Blanks.LocScope (locScopeHoistAnno)
import Blanks.Located (locatedVal)
import Blanks.Split (emptySplitState, splitLocScope)
import Blanks.Tracked (trackScope)
import Control.Monad.State.Strict (runState)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Blanks.Exp (CDecl (..), Decl (..), Ident, Level (..), declMapExp, declMapExpM, declToNameless,
                        runCDeclParser, synSpan)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Pretty.Simple (pPrint)

-- TODO
-- type ExpSplitScope l a = LocScope l Info (SplitFunctor (GlobalFunctor Exp))

chooseProgram :: [String]
chooseProgram =
  [ "(declare idInt (-> int int))"
  , "(define idInt (lambda (x) x))"
  , "(declare constInt (-> int (-> int int)))"
  , "(define constInt (lambda (x) (lambda (y) x)))"
  , "(declare choose (-> int (-> int int)))"
  , "(define choose (lambda (x) (let (b (zero? x)) (if b idInt (constInt x)))))"
  , "(declare chooseEx1 int)" -- should evaluate to 1
  , "(define chooseEx1 ((choose 0) 1))"
  , "(declare chooseEx2 int)" -- should evaluate to 2
  , "(define chooseEx2 ((choose 2) 3))"
  ]

-- Parse a sequence of definitions
-- Gather the names
-- Assign as globals
-- Split functions

testChooseProgram :: TestTree
testChooseProgram = testCase "choose program" $ do
  let numDefs = 5
  length chooseProgram @?= numDefs * 2
  cparsed <- traverse runCDeclParser chooseProgram
  let cdefns = [p | p <- cparsed, cdeclLvl p == LevelTerm]
      globals = Set.fromList (fmap cdeclName cdefns)
      cfier = predClassifier (`Set.member` globals)
      defns = fmap (declMapExp (globalScope cfier . locScopeHoistAnno (const ())) . locatedVal . declToNameless synSpan) cdefns
      dmap = Map.fromList (fmap (\d -> (declName d, d)) defns)
  Map.size dmap @?= numDefs
  -- putStrLn "*** INIT DECLS ***"
  -- pPrint dmap
  let (smap, st) = runState (traverse (declMapExpM (splitLocScope . trackScope)) dmap) emptySplitState
  Map.size smap @?= numDefs
  -- putStrLn "*** SPLIT STATE ***"
  -- pPrint st
  -- putStrLn "*** SPLIT DECLS ***"
  -- pPrint smap
  pure ()

testComplexExp :: TestTree
testComplexExp = testGroup "Complex Exp" [testChooseProgram]
