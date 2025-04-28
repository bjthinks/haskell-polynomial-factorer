module Tests where

import Test.HUnit
import Polynomial
import ParsePolynomial

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

parsePolynomialTests :: Test
parsePolynomialTests = TestList [
  "0" ~=? prettyPrint (parsePolynomial "0"),
  "1" ~=? prettyPrint (parsePolynomial "1"),
  "-1" ~=? prettyPrint (parsePolynomial "-1"),
  "5" ~=? prettyPrint (parsePolynomial "5"),
  "x" ~=? prettyPrint (parsePolynomial "x"),
  "-x" ~=? prettyPrint (parsePolynomial "-x"),
  "3x" ~=? prettyPrint (parsePolynomial "3x"),
  "-7x" ~=? prettyPrint (parsePolynomial "-7x"),
  "x+1" ~=? prettyPrint (parsePolynomial "1+x"),
  "x-1" ~=? prettyPrint (parsePolynomial "-1+x"),
  "-x+1" ~=? prettyPrint (parsePolynomial "1-x"),
  "-x-1" ~=? prettyPrint (parsePolynomial "-1-x"),
  "x^2" ~=? prettyPrint (parsePolynomial "x^2"),
  "x^2" ~=? prettyPrint (parsePolynomial "1x^2"),
  "-x^5" ~=? prettyPrint (parsePolynomial "-1x^5"),
  "-x^2+4x-4" ~=? prettyPrint (parsePolynomial "3x-2-x^2-2+x"),
  "1" ~=? prettyPrint (parsePolynomial "+1"),
  () ~=? ()]

p :: Polynomial
p = parsePolynomial "3x^2+5x-7"

q :: Polynomial
q = parsePolynomial "-8x+2"

r :: Polynomial
r = parsePolynomial "-x^3+2x^2+8x+3"

polynomialIsNum :: Test
polynomialIsNum = TestList [
  "57" ~=? prettyPrint (fromInteger 57),
  "2" ~=? prettyPrint (parsePolynomial "1" + parsePolynomial "1"),
  "3x^2-3x-5" ~=? prettyPrint (p+q),
  "-x^3+5x^2+13x-4" ~=? prettyPrint (p+r),
  "-x^3+2x^2+5" ~=? prettyPrint (q+r),
  "-3x^2-5x+7" ~=? prettyPrint (-p),
  "8x-2" ~=? prettyPrint (-q),
  "x^3-2x^2-8x-3" ~=? prettyPrint (-r),
  "-24x^3-34x^2+66x-14" ~=? prettyPrint (p*q),
  "-3x^5+x^4+41x^3+35x^2-41x-21" ~=? prettyPrint (p*r),
  "8x^4-18x^3-60x^2-8x+6" ~=? prettyPrint (q*r),
  "x^2-1" ~=? prettyPrint (parsePolynomial "x+1" * parsePolynomial "x-1"),
  () ~?= ()]

polynomialTests :: Test
polynomialTests = TestList
  [prettyPrintPolynomial, makePolynomialTests, parsePolynomialTests,
   polynomialIsNum]

tests :: Test
tests = TestList [polynomialTests]

runTests :: IO Counts
runTests = runTestTT tests
