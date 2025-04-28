module Tests where

import Test.HUnit
import Polynomial

prettyPrintPolynomial :: Test
prettyPrintPolynomial = TestList [
  "PrettyPrint 0" ~: "0" ~=? prettyPrint (Polynomial [(0,0)]),
  "PrettyPrint 1" ~: "1" ~=? prettyPrint (Polynomial [(1,0)]),
  "PrettyPrint 5" ~: "5" ~=? prettyPrint (Polynomial [(5,0)]),
  "PrettyPrint -1" ~: "-1" ~=? prettyPrint (Polynomial [(-1,0)]),
  "PrettyPrint -7" ~: "-7" ~=? prettyPrint (Polynomial [(-7,0)]),
  "PrettyPrint x" ~: "x" ~=? prettyPrint (Polynomial [(1,1)]),
  "PrettyPrint 3x" ~: "3x" ~=? prettyPrint (Polynomial [(3,1)]),
  "PrettyPrint -x" ~: "-x" ~=? prettyPrint (Polynomial [(-1,1)]),
  "PrettyPrint -5x" ~: "-5x" ~=? prettyPrint (Polynomial [(-5,1)]),
  "PrettyPrint x^2" ~: "x^2" ~=? prettyPrint (Polynomial [(1,2)]),
  "PrettyPrint 3x^2" ~: "3x^2" ~=? prettyPrint (Polynomial [(3,2)]),
  "PrettyPrint -x^2" ~: "-x^2" ~=? prettyPrint (Polynomial [(-1,2)]),
  "PrettyPrint -5x^2" ~: "-5x^2" ~=? prettyPrint (Polynomial [(-5,2)]),
  "PrettyPrint -53x^27" ~: "-53x^27" ~=? prettyPrint (Polynomial [(-53,27)]),
  "PrettyPrint x+1" ~: "x+1" ~=? prettyPrint (Polynomial [(1,1),(1,0)]),
  "PrettyPrint x-1" ~: "x-1" ~=? prettyPrint (Polynomial [(1,1),(-1,0)]),
  "PrettyPrint -x+1" ~: "-x+1" ~=? prettyPrint (Polynomial [(-1,1),(1,0)]),
  "PrettyPrint -x-1" ~: "-x-1" ~=? prettyPrint (Polynomial [(-1,1),(-1,0)]),
  "PrettyPrint 5x+3" ~: "5x+3" ~=? prettyPrint (Polynomial [(5,1),(3,0)]),
  "PrettyPrint 5x-3" ~: "5x-3" ~=? prettyPrint (Polynomial [(5,1),(-3,0)]),
  "PrettyPrint -5x+3" ~: "-5x+3" ~=? prettyPrint (Polynomial [(-5,1),(3,0)]),
  "PrettyPrint -5x-3" ~: "-5x-3" ~=? prettyPrint (Polynomial [(-5,1),(-3,0)]),
  "PrettyPrint x^2+x+1" ~: "x^2+x+1" ~=?
    prettyPrint (Polynomial [(1,2),(1,1),(1,0)]),
  "PrettyPrint -x^2+x+1" ~: "-x^2+x+1" ~=?
    prettyPrint (Polynomial [(-1,2),(1,1),(1,0)]),
  "PrettyPrint x^2-x+1" ~: "x^2-x+1" ~=?
    prettyPrint (Polynomial [(1,2),(-1,1),(1,0)]),
  "PrettyPrint 4x^2+4x+1" ~: "4x^2+4x+1" ~=?
    prettyPrint (Polynomial [(4,2),(4,1),(1,0)]),
  "PrettyPrint -4x^2-4x-1" ~: "-4x^2-4x-1" ~=?
    prettyPrint (Polynomial [(-4,2),(-4,1),(-1,0)]),
  () ~=? ()]

makePolynomialTests :: Test
makePolynomialTests = TestList [
  "0" ~=? prettyPrint (makePolynomial []),
  "0" ~=? prettyPrint (makePolynomial [(0,0)]),
  "0" ~=? prettyPrint (makePolynomial [(0,1)]),
  "0" ~=? prettyPrint (makePolynomial [(0,2)]),
  "5" ~=? prettyPrint (makePolynomial [(3,2),(5,0),(-3,2)]),
  "1" ~=? prettyPrint (makePolynomial [(1,0)]),
  "x+1" ~=? prettyPrint (makePolynomial [(1,0),(2,1),(-1,1)]),
  "x-1" ~=? prettyPrint (makePolynomial [(1,0),(2,1),(-2,0),(-1,1)]),
  "-x+1" ~=? prettyPrint (makePolynomial [(-2,1),(1,0),(2,1),(-1,1)]),
  "-x-1" ~=? prettyPrint (makePolynomial [(-2,1),(1,0),(2,1),(-1,1),(-2,0)]),
  () ~=? ()]

polynomialTests :: Test
polynomialTests = TestList
  [prettyPrintPolynomial, makePolynomialTests]

tests :: Test
tests = TestList [polynomialTests]

runTests :: IO Counts
runTests = runTestTT tests
