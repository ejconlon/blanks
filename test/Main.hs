module Main
  ( main
  ) where

import Test.Blanks.ExpTest (testExp)
import Test.Blanks.ScopeTest (testScope)
import Test.Blanks.SplitTest (testSplit)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain (testGroup "Blanks" [testScope, testExp, testSplit])
