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
  "Show -5x" ~: "-5x" ~=? show (Polynomial [(-5,1)]),
  "Show x^2" ~: "x^2" ~=? show (Polynomial [(1,2)]),
  "Show 3x^2" ~: "3x^2" ~=? show (Polynomial [(3,2)]),
  "Show -x^2" ~: "-x^2" ~=? show (Polynomial [(-1,2)]),
  "Show -5x^2" ~: "-5x^2" ~=? show (Polynomial [(-5,2)]),
  "Show -53x^27" ~: "-53x^27" ~=? show (Polynomial [(-53,27)]),
  "Show x+1" ~: "x+1" ~=? show (Polynomial [(1,1),(1,0)])
  ]

tests :: Test
tests = TestList [polynomialTests]

runTests :: IO Counts
runTests = runTestTT tests
