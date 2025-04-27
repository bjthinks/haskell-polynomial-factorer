module Tests where

import Test.HUnit
import Polynomial

polynomialTests :: Test
polynomialTests = TestList [
  "Show 1" ~: "1" ~=? show (Polynomial [(1,0)]),
  "Show x" ~: "x" ~=? show (Polynomial [(1,1)])
  ]

tests :: Test
tests = TestList [polynomialTests]

runTests :: IO Counts
runTests = runTestTT tests
