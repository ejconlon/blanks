module Test.Blanks.Tests.ScopeTest
  ( testScope
  ) where

import Blanks (SubError (..), scopeApply1, scopeInstantiate1)
import qualified Data.Set as Set
import Test.Blanks.Lib.Assertions ((@/=))
import Test.Blanks.Lib.SimpleScope (app, freeVars, lam, lets, sapp, sbase, sbase2, sbound, sconst, sflip, sfree, sfree2,
                                    sid, slet, sletFree, sletFree2, sletWonky, sletWonky2, svar, svar2, swonky, swonky2,
                                    var)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testScope :: TestTree
testScope =
  let testEq =
        testCase "eq" $ do
          svar @?= svar
          svar @/= svar2
          sid @?= lam 'x' (var 'x')
          sid @?= lam 'y' (var 'y')
          sid @/= lam 'x' (var 'y')
          sid @/= lam 'y' (var 'x')
          sid @/= svar
          sbase @?= sbase
          sbase @/= sbase2
          slet @?= lets 'z' sbase (var 'z')
          slet @?= lets 'y' sbase (var 'y')
          slet @/= lets 'y' sbase2 (var 'y')
          slet @/= lets 'y' sbase (var 'z')
          sletFree @?= lets 'z' sbase svar
          sletFree2 @?= lets 'z' svar sbase

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
          freeVars sapp @?= Set.singleton 'x'
          freeVars sbase @?= Set.empty
          freeVars slet @?= Set.empty
          freeVars sletFree @?= Set.singleton 'x'
          freeVars sletFree2 @?= Set.singleton 'x'

      testInstantiate =
        testCase "instantiate" $ do
          scopeInstantiate1 svar2 svar @?= svar
          scopeInstantiate1 svar2 sbound @?= svar2
          scopeInstantiate1 svar2 sid @?= sid
          scopeInstantiate1 svar2 swonky @?= swonky2
          scopeInstantiate1 svar2 sapp @?= app svar svar2
          scopeInstantiate1 svar2 sletWonky @?= lets 'y' sbase svar2
          scopeInstantiate1 svar2 sletWonky2 @?= lets 'y' svar2 sbase

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
          (sfree >>= const svar2) @?= lam 'y' svar2
          (sfree2 >>= const svar2) @?= lam 'c' (lam 'd' svar2)
          (sapp >>= const svar2) @?= app svar2 sbound
          (sletFree >>= const svar2) @?= lets 'y' sbase svar2
          (sletFree2 >>= const svar2) @?= lets 'y' svar2 sbase

      testIdSub =
        testCase "id sub" $ do
          (svar >>= const sid) @?= sid
          (sfree >>= const sid) @?= lam 'y' sid
          (sfree2 >>= const sid) @?= lam 'c' (lam 'd' sid)
          (sapp >>= const sid) @?= app sid sbound

   in testGroup "Scope" [testEq, testFreeVars, testInstantiate, testApply, testVarSub, testIdSub]
