module Tests where

import Test.HUnit
import Polynomial
import ParsePolynomial
import ModularPolynomial

printPolynomialTests :: Test
printPolynomialTests = TestList [
  "PrettyPrint 0" ~: "0" ~=? printPolynomial (Polynomial []),
  "PrettyPrint 1" ~: "1" ~=? printPolynomial (Polynomial [(1,0)]),
  "PrettyPrint 5" ~: "5" ~=? printPolynomial (Polynomial [(5,0)]),
  "PrettyPrint -1" ~: "-1" ~=? printPolynomial (Polynomial [(-1,0)]),
  "PrettyPrint -7" ~: "-7" ~=? printPolynomial (Polynomial [(-7,0)]),
  "PrettyPrint x" ~: "x" ~=? printPolynomial (Polynomial [(1,1)]),
  "PrettyPrint 3x" ~: "3x" ~=? printPolynomial (Polynomial [(3,1)]),
  "PrettyPrint -x" ~: "-x" ~=? printPolynomial (Polynomial [(-1,1)]),
  "PrettyPrint -5x" ~: "-5x" ~=? printPolynomial (Polynomial [(-5,1)]),
  "PrettyPrint x^2" ~: "x^2" ~=? printPolynomial (Polynomial [(1,2)]),
  "PrettyPrint 3x^2" ~: "3x^2" ~=? printPolynomial (Polynomial [(3,2)]),
  "PrettyPrint -x^2" ~: "-x^2" ~=? printPolynomial (Polynomial [(-1,2)]),
  "PrettyPrint -5x^2" ~: "-5x^2" ~=? printPolynomial (Polynomial [(-5,2)]),
  "PrettyPrint -53x^27" ~: "-53x^27" ~=?
    printPolynomial (Polynomial [(-53,27)]),
  "PrettyPrint x+1" ~: "x+1" ~=? printPolynomial (Polynomial [(1,1),(1,0)]),
  "PrettyPrint x-1" ~: "x-1" ~=? printPolynomial (Polynomial [(1,1),(-1,0)]),
  "PrettyPrint -x+1" ~: "-x+1" ~=? printPolynomial (Polynomial [(-1,1),(1,0)]),
  "PrettyPrint -x-1" ~: "-x-1" ~=? printPolynomial (Polynomial [(-1,1),(-1,0)]),
  "PrettyPrint 5x+3" ~: "5x+3" ~=? printPolynomial (Polynomial [(5,1),(3,0)]),
  "PrettyPrint 5x-3" ~: "5x-3" ~=? printPolynomial (Polynomial [(5,1),(-3,0)]),
  "PrettyPrint -5x+3" ~: "-5x+3" ~=?
    printPolynomial (Polynomial [(-5,1),(3,0)]),
  "PrettyPrint -5x-3" ~: "-5x-3" ~=?
    printPolynomial (Polynomial [(-5,1),(-3,0)]),
  "PrettyPrint x^2+x+1" ~: "x^2+x+1" ~=?
    printPolynomial (Polynomial [(1,2),(1,1),(1,0)]),
  "PrettyPrint -x^2+x+1" ~: "-x^2+x+1" ~=?
    printPolynomial (Polynomial [(-1,2),(1,1),(1,0)]),
  "PrettyPrint x^2-x+1" ~: "x^2-x+1" ~=?
    printPolynomial (Polynomial [(1,2),(-1,1),(1,0)]),
  "PrettyPrint 4x^2+4x+1" ~: "4x^2+4x+1" ~=?
    printPolynomial (Polynomial [(4,2),(4,1),(1,0)]),
  "PrettyPrint -4x^2-4x-1" ~: "-4x^2-4x-1" ~=?
    printPolynomial (Polynomial [(-4,2),(-4,1),(-1,0)]),
  () ~=? ()]

makePolynomialTests :: Test
makePolynomialTests = TestList [
  "0" ~=? printPolynomial (makePolynomial []),
  "0" ~=? printPolynomial (makePolynomial [(0,0)]),
  "0" ~=? printPolynomial (makePolynomial [(0,1)]),
  "0" ~=? printPolynomial (makePolynomial [(0,2)]),
  "5" ~=? printPolynomial (makePolynomial [(3,2),(5,0),(-3,2)]),
  "1" ~=? printPolynomial (makePolynomial [(1,0)]),
  "x+1" ~=? printPolynomial (makePolynomial [(1,0),(2,1),(-1,1)]),
  "x-1" ~=? printPolynomial (makePolynomial [(1,0),(2,1),(-2,0),(-1,1)]),
  "-x+1" ~=? printPolynomial (makePolynomial [(-2,1),(1,0),(2,1),(-1,1)]),
  "-x-1" ~=? printPolynomial
    (makePolynomial [(-2,1),(1,0),(2,1),(-1,1),(-2,0)]),
  () ~=? ()]

parsePolynomialTests :: Test
parsePolynomialTests = TestList [
  "0" ~=? printPolynomial (parsePolynomial "0"),
  "1" ~=? printPolynomial (parsePolynomial "1"),
  "-1" ~=? printPolynomial (parsePolynomial "-1"),
  "5" ~=? printPolynomial (parsePolynomial "5"),
  "x" ~=? printPolynomial (parsePolynomial "x"),
  "-x" ~=? printPolynomial (parsePolynomial "-x"),
  "3x" ~=? printPolynomial (parsePolynomial "3x"),
  "-7x" ~=? printPolynomial (parsePolynomial "-7x"),
  "x+1" ~=? printPolynomial (parsePolynomial "1+x"),
  "x-1" ~=? printPolynomial (parsePolynomial "-1+x"),
  "-x+1" ~=? printPolynomial (parsePolynomial "1-x"),
  "-x-1" ~=? printPolynomial (parsePolynomial "-1-x"),
  "x^2" ~=? printPolynomial (parsePolynomial "x^2"),
  "x^2" ~=? printPolynomial (parsePolynomial "1x^2"),
  "-x^5" ~=? printPolynomial (parsePolynomial "-1x^5"),
  "-x^2+4x-4" ~=? printPolynomial (parsePolynomial "3x-2-x^2-2+x"),
  "1" ~=? printPolynomial (parsePolynomial "+1"),
  () ~=? ()]

p :: Polynomial
p = parsePolynomial "3x^2+5x-7"

q :: Polynomial
q = parsePolynomial "-8x+2"

r :: Polynomial
r = parsePolynomial "-x^3+2x^2+8x+3"

polynomialIsNum :: Test
polynomialIsNum = TestList [
  "57" ~=? printPolynomial (fromInteger 57),
  "2" ~=? printPolynomial (parsePolynomial "1" + parsePolynomial "1"),
  "3x^2-3x-5" ~=? printPolynomial (p+q),
  "-x^3+5x^2+13x-4" ~=? printPolynomial (p+r),
  "-x^3+2x^2+5" ~=? printPolynomial (q+r),
  "-3x^2-5x+7" ~=? printPolynomial (-p),
  "8x-2" ~=? printPolynomial (-q),
  "x^3-2x^2-8x-3" ~=? printPolynomial (-r),
  "-24x^3-34x^2+66x-14" ~=? printPolynomial (p*q),
  "-3x^5+x^4+41x^3+35x^2-41x-21" ~=? printPolynomial (p*r),
  "8x^4-18x^3-60x^2-8x+6" ~=? printPolynomial (q*r),
  "x^2-1" ~=? printPolynomial (parsePolynomial "x+1" * parsePolynomial "x-1"),
  "x^4-4x^3+6x^2-4x+1" ~=? printPolynomial (parsePolynomial "x-1" ^ (4 :: Int)),
  () ~?= ()]

polynomialTests :: Test
polynomialTests = TestList
  [printPolynomialTests, makePolynomialTests, parsePolynomialTests,
   polynomialIsNum]

printModularPolynomialTests :: Test
printModularPolynomialTests = TestList [
  "0 mod 5" ~=? printModularPolynomial (ModularPolynomial 5 []),
  "3 mod 5" ~=? printModularPolynomial (ModularPolynomial 5 [(3,0)]),
  "x^2 mod 5" ~=? printModularPolynomial (ModularPolynomial 5 [(1,2)]),
  () ~?= ()]

makeModularPolynomialTests :: Test
makeModularPolynomialTests = TestList [
  "4x^2+3 mod 5" ~=? printModularPolynomial
    (makeModularPolynomial 5 [(-2,0),(3,1),(7,1),(-1,2)]),
  () ~?= ()]

{-
mp :: ModularPolynomial
mp = parseModularPolynomial "3x^2+5x-7 mod 5"

mq :: ModularPolynomial
mq = parseModularPolynomial "-8x+2 mod 5"

mr :: ModularPolynomial
mr = parseModularPolynomial "-x^3+2x^2+8x+3 mod 5"
-}

modularPolynomialTests :: Test
modularPolynomialTests = TestList [printModularPolynomialTests]

tests :: Test
tests = TestList [polynomialTests, modularPolynomialTests]

runTests :: IO Counts
runTests = runTestTT tests
