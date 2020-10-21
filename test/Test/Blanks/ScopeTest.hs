module Test.Blanks.ScopeTest
  ( testScope
  ) where

import Blanks (Name (..), NameOnly, Scope, pattern ScopeBound, locScopeForget, scopeAbstract1, scopeApply1, scopeInstantiate1, trackScope)
import Control.Monad.Identity (Identity (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Blanks.Assertions ((@/=))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

type BareScope = Scope (NameOnly Char) Identity Char

abst :: Char -> BareScope -> BareScope
abst a = scopeAbstract1 (Name a ()) a

bound :: Int -> BareScope
bound = ScopeBound

var :: Char -> BareScope
var = pure

freeVars :: BareScope -> Set Char
freeVars = foldMap Set.singleton

assertFreeVars :: BareScope -> Set Char -> Assertion
assertFreeVars s expected = do
  let actual = freeVars s
  actual @?= expected
  let (actual', ls) = trackScope s
  actual' @?= expected
  let s' = locScopeForget ls
  s' @?= s

testScope :: TestTree
testScope =
  let svar = var 'x'
      sbound = bound 0
      sfree = abst 'y' (var 'x')
      sfree2 = abst 'z' (abst 'y' (var 'x'))
      sid = abst 'x' (var 'x')
      swonky = abst 'x' (bound 0)
      sconst = abst 'x' (abst 'y' (var 'x'))
      sflip = abst 'x' (abst 'y' (var 'y'))
      svar2 = var 'e'
      swonky2 = abst 'x' svar2

      testEq =
        testCase "eq" $ do
          svar @?= svar
          svar @/= svar2
          sid @?= abst 'x' (var 'x')
          sid @?= abst 'y' (var 'y')
          sid @/= abst 'x' (var 'y')
          sid @/= abst 'y' (var 'x')
          sid @/= svar

      testFreeVars =
        testCase "free vars" $ do
          assertFreeVars svar (Set.singleton 'x')
          assertFreeVars sbound Set.empty
          assertFreeVars sfree (Set.singleton 'x')
          assertFreeVars sfree2 (Set.singleton 'x')
          assertFreeVars sid Set.empty
          assertFreeVars swonky Set.empty
          assertFreeVars sconst Set.empty
          assertFreeVars sflip Set.empty
          assertFreeVars svar2 (Set.singleton 'e')
          assertFreeVars swonky2 (Set.singleton 'e')

      testInstantiate =
        testCase "instantiate" $ do
          scopeInstantiate1 svar2 svar @?= svar
          scopeInstantiate1 svar2 sbound @?= svar2
          scopeInstantiate1 svar2 sid @?= sid
          scopeInstantiate1 svar2 swonky @?= swonky2

      testApply =
        testCase "apply" $ do
          scopeApply1 svar2 sid @?= Right svar2
          scopeApply1 svar2 swonky @?= Right sbound
          scopeApply1 svar2 sconst @?= Right swonky2
          scopeApply1 svar2 sflip @?= Right sid

      testVarSub =
        testCase "var sub" $ do
          (svar >>= const svar2) @?= svar2
          (sfree >>= const svar2) @?= abst 'y' svar2
          (sfree2 >>= const svar2) @?= abst 'c' (abst 'd' svar2)

      testIdSub =
        testCase "id sub" $ do
          (svar >>= const sid) @?= sid
          (sfree >>= const sid) @?= abst 'y' sid
          (sfree2 >>= const sid) @?= abst 'c' (abst 'd' sid)

   in testGroup "Scope" [testEq, testFreeVars, testInstantiate, testApply, testVarSub, testIdSub]
