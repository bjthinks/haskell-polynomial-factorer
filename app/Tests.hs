module Tests where

import Test.HUnit
import Polynomial

polynomialTests :: Test
polynomialTests = TestList [
  "Show 0" ~: "0" ~=? show (Polynomial [(0,0)]),
  "Show 1" ~: "1" ~=? show (Polynomial [(1,0)]),
  "Show 5" ~: "5" ~=? show (Polynomial [(5,0)]),
  "Show -1" ~: "-1" ~=? show (Polynomial [(-1,0)]),
  "Show -7" ~: "-7" ~=? show (Polynomial [(-7,0)]),
  "Show x" ~: "x" ~=? show (Polynomial [(1,1)]),
  "Show 3x" ~: "3x" ~=? show (Polynomial [(3,1)]),
  "Show -x" ~: "-x" ~=? show (Polynomial [(-1,1)]),
  "Show -5x" ~: "-5x" ~=? show (Polynomial [(-5,1)])
  ]

tests :: Test
tests = TestList [polynomialTests]

runTests :: IO Counts
runTests = runTestTT tests
