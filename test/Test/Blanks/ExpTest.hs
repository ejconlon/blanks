module Test.Blanks.ExpTest where

import Blanks (pattern NameOnly, pattern ScopeBinder, pattern ScopeBound, pattern ScopeEmbed, pattern ScopeFree,
               locScopeForget, locScopeLocation)
import Control.DeepSeq (force)
import Test.Blanks.Exp (Exp (..), ExpScope, Ident (..), cexpLoc, cexpParser, named, nameless)
import Test.Blanks.Parsing (runParserIO)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testSingle :: TestName -> String -> ExpScope -> TestTree
testSingle name input expected = testCase name $ do
  namedExp <- runParserIO cexpParser input
  -- Force here just to test that we can
  let namelessExp = force (nameless namedExp)
  cexpLoc namedExp @?= locScopeLocation namelessExp
  let actual = locScopeForget namelessExp
  expected @?= actual
  let renamedExp = named namelessExp
  Just namedExp @?= renamedExp

testExp :: TestTree
testExp = testGroup "Exp" cases where
  xIdent = Ident "x"
  yIdent = Ident "y"
  xExp = ScopeFree xIdent
  yExp = ScopeFree yIdent
  trueExp = ScopeEmbed (ExpBool True)
  intExp = ScopeEmbed (ExpInt 42)
  negIntExp = ScopeEmbed (ExpInt (-42))
  boolTyExp = ScopeEmbed ExpTyBool
  intTyExp = ScopeEmbed ExpTyInt
  cases =
    [ testSingle "var" "x" xExp
    , testSingle "true" "#t" trueExp
    , testSingle "false" "#f" (ScopeEmbed (ExpBool False))
    , testSingle "int" "42" intExp
    , testSingle "neg int" "-42" negIntExp
    , testSingle "add" "(+ 42 -42)" (ScopeEmbed (ExpAdd intExp negIntExp))
    , testSingle "if" "(if #t 42 -42)" (ScopeEmbed (ExpIf trueExp intExp negIntExp))
    , testSingle "add var" "(+ 42 x)" (ScopeEmbed (ExpAdd intExp xExp))
    , testSingle "iszero" "(zero? 42)" (ScopeEmbed (ExpIsZero intExp))
    , testSingle "app" "(x y)" (ScopeEmbed (ExpApp xExp yExp))
    , testSingle "abs yy" "(lambda (y) y)" (ScopeBinder 1 (NameOnly yIdent) (ScopeBound 0))
    , testSingle "abs xyy" "(lambda (x) (lambda (y) y))" (ScopeBinder 1 (NameOnly xIdent) (ScopeBinder 1 (NameOnly yIdent) (ScopeBound 0)))
    , testSingle "abs xyx" "(lambda (x) (lambda (y) x))" (ScopeBinder 1 (NameOnly xIdent) (ScopeBinder 1 (NameOnly yIdent) (ScopeBound 1)))
    , testSingle "app abs" "((lambda (x) x) 42)" (ScopeEmbed (ExpApp (ScopeBinder 1 (NameOnly xIdent) (ScopeBound 0)) intExp))
    , testSingle "ty bool" "bool" boolTyExp
    , testSingle "ty int" "int" intTyExp
    , testSingle "ty fun" "(-> int bool)" (ScopeEmbed (ExpTyFun intTyExp boolTyExp))
    , testSingle "asc" "(: 42 int)" (ScopeEmbed (ExpAsc intExp intTyExp))
    ]
