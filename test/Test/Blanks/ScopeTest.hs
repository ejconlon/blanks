module Test.Blanks.ScopeTest
  ( testScope
  ) where

import Blanks (SubError (..), scopeApply1, scopeInstantiate1)
import qualified Data.Set as Set
import Test.Blanks.Assertions ((@/=))
import Test.Blanks.SimpleScope (abst, embed, freeVars, sbound, sconst, sflip, sfree, sfree2, sid, spair, svar, svar2,
                                swonky, swonky2, var)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testScope :: TestTree
testScope =
  let testEq =
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
          freeVars svar @?= Set.singleton 'x'
          freeVars sbound @?= Set.empty
          freeVars sfree @?= Set.singleton 'x'
          freeVars sfree2 @?= Set.singleton 'x'
          freeVars sid @?= Set.empty
          freeVars swonky @?= Set.empty
          freeVars sconst @?= Set.empty
          freeVars sflip @?= Set.empty
          freeVars svar2 @?= Set.singleton 'e'
          freeVars swonky2 @?= Set.singleton 'e'
          freeVars spair @?= Set.singleton 'x'

      testInstantiate =
        testCase "instantiate" $ do
          scopeInstantiate1 svar2 svar @?= svar
          scopeInstantiate1 svar2 sbound @?= svar2
          scopeInstantiate1 svar2 sid @?= sid
          scopeInstantiate1 svar2 swonky @?= swonky2
          scopeInstantiate1 svar2 spair @?= embed svar svar2

      testApply =
        testCase "apply" $ do
          scopeApply1 svar2 sid @?= Right svar2
          scopeApply1 svar2 swonky @?= Right sbound
          scopeApply1 svar2 sconst @?= Right swonky2
          scopeApply1 svar2 sflip @?= Right sid
          scopeApply1 svar2 svar @?= Left NonBinderError

      testVarSub =
        testCase "var sub" $ do
          (svar >>= const svar2) @?= svar2
          (sfree >>= const svar2) @?= abst 'y' svar2
          (sfree2 >>= const svar2) @?= abst 'c' (abst 'd' svar2)
          (spair >>= const svar2) @?= embed svar2 sbound

      testIdSub =
        testCase "id sub" $ do
          (svar >>= const sid) @?= sid
          (sfree >>= const sid) @?= abst 'y' sid
          (sfree2 >>= const sid) @?= abst 'c' (abst 'd' sid)
          (spair >>= const sid) @?= embed sid sbound

   in testGroup "Scope" [testEq, testFreeVars, testInstantiate, testApply, testVarSub, testIdSub]
