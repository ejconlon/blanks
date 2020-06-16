module Main
  ( main
  ) where

import Test.Blanks.LocScopeTest (testLocScope)
import Test.Blanks.ScopeTest (testScope)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain (testGroup "Blanks" [testScope, testLocScope])
