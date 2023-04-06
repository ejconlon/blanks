module Main
  ( main
  )
where

import Test.Blanks.Tests.ExpTest (testExp)
import Test.Blanks.Tests.LiftTest (testLift)
import Test.Blanks.Tests.ProgramTest (testProgram)
import Test.Blanks.Tests.ScopeTest (testScope)
import Test.Blanks.Tests.TrackedTest (testTracked)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Blanks"
      [ testScope
      , testTracked
      , testExp
      , testLift
      , testProgram
      ]
