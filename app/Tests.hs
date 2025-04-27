module Tests where

import Test.HUnit
import Polynomial

showPolynomial :: Test
showPolynomial = TestList [
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
  "Show x+1" ~: "x+1" ~=? show (Polynomial [(1,1),(1,0)]),
  "Show x-1" ~: "x-1" ~=? show (Polynomial [(1,1),(-1,0)]),
  "Show -x+1" ~: "-x+1" ~=? show (Polynomial [(-1,1),(1,0)]),
  "Show -x-1" ~: "-x-1" ~=? show (Polynomial [(-1,1),(-1,0)]),
  "Show 5x+3" ~: "5x+3" ~=? show (Polynomial [(5,1),(3,0)]),
  "Show 5x-3" ~: "5x-3" ~=? show (Polynomial [(5,1),(-3,0)]),
  "Show -5x+3" ~: "-5x+3" ~=? show (Polynomial [(-5,1),(3,0)]),
  "Show -5x-3" ~: "-5x-3" ~=? show (Polynomial [(-5,1),(-3,0)]),
  "Show x^2+x+1" ~: "x^2+x+1" ~=? show (Polynomial [(1,2),(1,1),(1,0)]),
  "Show -x^2+x+1" ~: "-x^2+x+1" ~=? show (Polynomial [(-1,2),(1,1),(1,0)]),
  "Show x^2-x+1" ~: "x^2-x+1" ~=? show (Polynomial [(1,2),(-1,1),(1,0)]),
  "Show 4x^2+4x+1" ~: "4x^2+4x+1" ~=? show (Polynomial [(4,2),(4,1),(1,0)]),
  "Show -4x^2-4x-1" ~: "-4x^2-4x-1" ~=?
    show (Polynomial [(-4,2),(-4,1),(-1,0)]),
  () ~=? ()]

polynomialTests :: Test
polynomialTests = TestList [showPolynomial]

tests :: Test
tests = TestList [polynomialTests]

runTests :: IO Counts
runTests = runTestTT tests
