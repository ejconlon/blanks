module Test.Blanks.ComplexExpTest
  ( testComplexExp
  ) where

import Test.Blanks.Exp (runCDeclParser)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

-- TODO
-- type ExpSplitScope l a = LocScope l Info (SplitFunctor (GlobalFunctor Exp))

fooProgram :: [String]
fooProgram =
  [ "(declare idInt (-> int int))"
  , "(define idInt (lambda (x) x))"
  , "(declare constInt (-> int (-> int int)))"
  , "(define constInt (lambda (x) (lambda (y) x)))"
  , "(declare foo (-> int (-> int int)))"
  , "(define foo (lambda (x) (let (b (zero? x)) (if b idInt (constInt x)))))"
  , "(declare fooEx1 int)" -- should evaluate to 1
  , "(define fooEx1 ((foo 0) 1))"
  , "(declare fooEx2 int)" -- should evaluate to 2
  , "(define fooEx2 ((foo 2) 3))"
  ]

-- Parse a sequence of definitions
-- Gather the names
-- Assign as globals
-- Split functions

testFooProgram :: TestTree
testFooProgram = testCase "foo program" $ do
  parsed <- traverse runCDeclParser fooProgram
  print parsed
  pure ()

testComplexExp :: TestTree
testComplexExp = testGroup "Complex Exp" [testFooProgram]
