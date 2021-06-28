module Test.Blanks.Tests.ProgramTest
  ( testProgram
  ) where

import Blanks (emptyLiftState, globalScope, locScopeHoistAnno, locatedVal, predClassifier, predLiftLocScope, runState,
               trackScope)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Blanks.Lib.Exp (CDecl (..), Decl (..), Level (..), declMapExp, declMapExpM, declToNameless, infoShouldLift,
                            runCDeclParser, synSpan)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
-- import Text.Pretty.Simple (pPrint)

-- TODO
-- type ExpLiftScope l a = LocScope l Info (LiftFunctor (GlobalFunctor Exp))

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
-- Lift functions

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
  let (smap, _) = runState (traverse (declMapExpM (predLiftLocScope infoShouldLift . trackScope)) dmap) emptyLiftState
  Map.size smap @?= numDefs
  -- putStrLn "*** SPLIT STATE ***"
  -- pPrint st
  -- putStrLn "*** SPLIT DECLS ***"
  -- pPrint smap
  pure ()

testProgram :: TestTree
testProgram = testGroup "Program" [testChooseProgram]
