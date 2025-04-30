module Tests where

import Data.List
import Test.HUnit
import Defs
import Polynomial
import ParsePolynomial
import ModularPolynomial
import SquareFree

printPolynomialTests :: Test
printPolynomialTests = TestList [
  "PrettyPrint 0" ~: "0" ~=? printPolynomial (Polynomial []),
  "PrettyPrint 1" ~: "1" ~=? printPolynomial (Polynomial [Term 1 0]),
  "PrettyPrint 5" ~: "5" ~=? printPolynomial (Polynomial [Term 5 0]),
  "PrettyPrint -1" ~: "-1" ~=? printPolynomial (Polynomial [Term (-1) 0]),
  "PrettyPrint -7" ~: "-7" ~=? printPolynomial (Polynomial [Term (-7) 0]),
  "PrettyPrint x" ~: "x" ~=? printPolynomial (Polynomial [Term 1 1]),
  "PrettyPrint 3x" ~: "3x" ~=? printPolynomial (Polynomial [Term 3 1]),
  "PrettyPrint -x" ~: "-x" ~=? printPolynomial (Polynomial [Term (-1) 1]),
  "PrettyPrint -5x" ~: "-5x" ~=? printPolynomial (Polynomial [Term (-5) 1]),
  "PrettyPrint x^2" ~: "x^2" ~=? printPolynomial (Polynomial [Term 1 2]),
  "PrettyPrint 3x^2" ~: "3x^2" ~=? printPolynomial (Polynomial [Term 3 2]),
  "PrettyPrint -x^2" ~: "-x^2" ~=? printPolynomial (Polynomial [Term (-1) 2]),
  "PrettyPrint -5x^2" ~: "-5x^2" ~=? printPolynomial (Polynomial [Term (-5) 2]),
  "PrettyPrint -53x^27" ~: "-53x^27" ~=?
    printPolynomial (Polynomial [Term (-53) 27]),
  "PrettyPrint x+1" ~: "x + 1" ~=?
    printPolynomial (Polynomial [Term 1 1, Term 1 0]),
  "PrettyPrint x-1" ~: "x - 1" ~=?
    printPolynomial (Polynomial [Term 1 1, Term (-1) 0]),
  "PrettyPrint -x+1" ~: "-x + 1" ~=?
    printPolynomial (Polynomial [Term (-1) 1, Term 1 0]),
  "PrettyPrint -x-1" ~: "-x - 1" ~=?
    printPolynomial (Polynomial [Term (-1) 1, Term (-1) 0]),
  "PrettyPrint 5x+3" ~: "5x + 3" ~=?
    printPolynomial (Polynomial [Term 5 1, Term 3 0]),
  "PrettyPrint 5x-3" ~: "5x - 3" ~=?
    printPolynomial (Polynomial [Term 5 1, Term (-3) 0]),
  "PrettyPrint -5x+3" ~: "-5x + 3" ~=?
    printPolynomial (Polynomial [Term (-5) 1, Term 3 0]),
  "PrettyPrint -5x-3" ~: "-5x - 3" ~=?
    printPolynomial (Polynomial [Term (-5) 1, Term (-3) 0]),
  "PrettyPrint x^2+x+1" ~: "x^2 + x + 1" ~=?
    printPolynomial (Polynomial [Term 1 2, Term 1 1, Term 1 0]),
  "PrettyPrint -x^2+x+1" ~: "-x^2 + x + 1" ~=?
    printPolynomial (Polynomial [Term (-1) 2, Term 1 1, Term 1 0]),
  "PrettyPrint x^2-x+1" ~: "x^2 - x + 1" ~=?
    printPolynomial (Polynomial [Term 1 2, Term (-1) 1, Term 1 0]),
  "PrettyPrint 4x^2+4x+1" ~: "4x^2 + 4x + 1" ~=?
    printPolynomial (Polynomial [Term 4 2, Term 4 1, Term 1 0]),
  "PrettyPrint -4x^2-4x-1" ~: "-4x^2 - 4x - 1" ~=?
    printPolynomial (Polynomial [Term (-4) 2, Term (-4) 1, Term (-1) 0]),
  () ~=? ()]

makePolynomialTests :: Test
makePolynomialTests = TestList [
  "0" ~=? printPolynomial (makePolynomial []),
  "0" ~=? printPolynomial (makePolynomial [Term 0 0]),
  "0" ~=? printPolynomial (makePolynomial [Term 0 1]),
  "0" ~=? printPolynomial (makePolynomial [Term 0 2]),
  "5" ~=? printPolynomial (makePolynomial [Term 3 2, Term 5 0, Term (-3) 2]),
  "1" ~=? printPolynomial (makePolynomial [Term 1 0]),
  "x + 1" ~=? printPolynomial
    (makePolynomial [Term 1 0, Term 2 1, Term (-1) 1]),
  "x - 1" ~=? printPolynomial
    (makePolynomial [Term 1 0, Term 2 1, Term (-2) 0, Term (-1) 1]),
  "-x + 1" ~=? printPolynomial
    (makePolynomial [Term (-2) 1, Term 1 0, Term 2 1, Term (-1) 1]),
  "-x - 1" ~=? printPolynomial
    (makePolynomial [Term (-2) 1, Term 1 0, Term 2 1, Term (-1) 1,
                     Term (-2) 0]),
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
  "x + 1" ~=? printPolynomial (parsePolynomial "1+x"),
  "x - 1" ~=? printPolynomial (parsePolynomial "-1+x"),
  "-x + 1" ~=? printPolynomial (parsePolynomial "1-x"),
  "-x - 1" ~=? printPolynomial (parsePolynomial "-1-x"),
  "x^2" ~=? printPolynomial (parsePolynomial "x^2"),
  "x^2" ~=? printPolynomial (parsePolynomial "1x^2"),
  "-x^5" ~=? printPolynomial (parsePolynomial "-1x^5"),
  "-x^2 + 4x - 4" ~=? printPolynomial (parsePolynomial "3x-2-x^2-2+x"),
  "1" ~=? printPolynomial (parsePolynomial "+1"),
  () ~=? ()]

p :: Polynomial
p = parsePolynomial "  3   x ^  2 + 5 x   - 7  "

q :: Polynomial
q = parsePolynomial "-8x+2"

r :: Polynomial
r = parsePolynomial "-x^3+2x^2+8x+3"

polynomialIsNum :: Test
polynomialIsNum = TestList [
  "57" ~=? printPolynomial (fromInteger 57),
  "2" ~=? printPolynomial (parsePolynomial "1" + parsePolynomial "1"),
  "3x^2 - 3x - 5" ~=? printPolynomial (p+q),
  "-x^3 + 5x^2 + 13x - 4" ~=? printPolynomial (p+r),
  "-x^3 + 2x^2 + 5" ~=? printPolynomial (q+r),
  "-3x^2 - 5x + 7" ~=? printPolynomial (-p),
  "8x - 2" ~=? printPolynomial (-q),
  "x^3 - 2x^2 - 8x - 3" ~=? printPolynomial (-r),
  "-24x^3 - 34x^2 + 66x - 14" ~=? printPolynomial (p*q),
  "-3x^5 + x^4 + 41x^3 + 35x^2 - 41x - 21" ~=? printPolynomial (p*r),
  "8x^4 - 18x^3 - 60x^2 - 8x + 6" ~=? printPolynomial (q*r),
  "x^2 - 1" ~=? printPolynomial (parsePolynomial "x+1" * parsePolynomial "x-1"),
  "x^4 - 4x^3 + 6x^2 - 4x + 1" ~=?
    printPolynomial (parsePolynomial "x-1" ^ (4 :: Int)),
  () ~=? ()]

polynomialTests :: Test
polynomialTests = TestList
  [printPolynomialTests, makePolynomialTests, parsePolynomialTests,
   polynomialIsNum]

printModularPolynomialTests :: Test
printModularPolynomialTests = TestList [
  "0 mod 5" ~=? printModularPolynomial (ModularPolynomial 5 []),
  "3 mod 5" ~=? printModularPolynomial (ModularPolynomial 5 [Term 3 0]),
  "x^2 mod 5" ~=? printModularPolynomial (ModularPolynomial 5 [Term 1 2]),
  () ~=? ()]

makeModularPolynomialTests :: Test
makeModularPolynomialTests = TestList [
  "4x^2 + 3 mod 5" ~=? printModularPolynomial
    (makeModularPolynomial 5 [Term (-2) 0, Term 3 1, Term 7 1, Term (-1) 2]),
  () ~=? ()]

mp :: ModularPolynomial
mp = parseModularPolynomial " 3 x ^ 2 + 5 x - 7 mod 5 "

mq :: ModularPolynomial
mq = parseModularPolynomial "-8x+2 mod 5"

mr :: ModularPolynomial
mr = parseModularPolynomial "-x^3+2x^2+8x+3 mod 5"

parseModularPolynomialTests :: Test
parseModularPolynomialTests = TestList [
  "3x^2 + 3 mod 5" ~=? printModularPolynomial mp,
  "2x + 2 mod 5" ~=? printModularPolynomial mq,
  "4x^3 + 2x^2 + 3x + 3 mod 5" ~=? printModularPolynomial mr,
  () ~=? ()]

modularPolynomialIsNum :: Test
modularPolynomialIsNum = TestList [
  "3x^2 + 2x mod 5" ~=? printModularPolynomial (mp + mq),
  "4x^3 + 3x + 1 mod 5" ~=? printModularPolynomial (mp + mr),
  "4x^3 + 2x^2 mod 5" ~=? printModularPolynomial (mq + mr),
  "x^3 + x^2 + x + 1 mod 5" ~=? printModularPolynomial (mp * mq),
  "2x^5 + x^4 + x^3 + 4x + 4 mod 5" ~=? printModularPolynomial (mp * mr),
  "3x^4 + 2x^3 + 2x + 1 mod 5" ~=? printModularPolynomial (mq * mr),
  "2x^2 + 2 mod 5" ~=? printModularPolynomial (negate mp),
  "3x + 3 mod 5" ~=? printModularPolynomial (negate mq),
  "x^3 + 3x^2 + 2x + 2 mod 5" ~=? printModularPolynomial (negate mr),
  () ~=? ()]

modularPolynomialTests :: Test
modularPolynomialTests = TestList
  [printModularPolynomialTests, makeModularPolynomialTests,
    parseModularPolynomialTests, modularPolynomialIsNum]

printResults :: [(Polynomial, Exponent)] -> [(String, Exponent)]
printResults = map (\(poly,expo) -> (printPolynomial poly,expo)) . sortOn snd

p1 :: Polynomial
p1 = parsePolynomial "x+1"

p2 :: Polynomial
p2 = parsePolynomial "x-1"

p3 :: Polynomial
p3 = parsePolynomial "x^2-2"

p4 :: Polynomial
p4 = parsePolynomial "x^3+3"

p5 :: Polynomial
p5 = parsePolynomial "x+5"

p6 :: Polynomial
p6 = parsePolynomial "10x^3"

p7 :: Polynomial
p7 = parsePolynomial "16"

p8 :: Polynomial
p8 = parsePolynomial "x^3 - x^2 - x + 1"

p9 :: Polynomial
p9 = parsePolynomial "3x^2 - 2x - 1"

p10 :: Polynomial
p10 = parsePolynomial "2x-2"

p11 :: Polynomial
p11 = parsePolynomial "3x+3"

polynomialOpTests :: Test
polynomialOpTests = TestList [
  "6x + 5" ~=? printPolynomial (derivative p),
  "-8" ~=? printPolynomial (derivative q),
  "-3x^2 + 4x + 8" ~=? printPolynomial (derivative r),
  "1" ~=? printPolynomial (derivative p1),
  "1" ~=? printPolynomial (derivative p2),
  "2x" ~=? printPolynomial (derivative p3),
  "3x^2" ~=? printPolynomial (derivative p4),
  "1" ~=? printPolynomial (derivative p5),
  Term 3 2 ~=? leadingTerm p,
  Term (-8) 1 ~=? leadingTerm q,
  Term (-1) 3 ~=? leadingTerm r,
  Term 1 1 ~=? leadingTerm p1,
  Term 1 1 ~=? leadingTerm p2,
  Term 1 2 ~=? leadingTerm p3,
  Term 1 3 ~=? leadingTerm p4,
  Term 1 1 ~=? leadingTerm p5,
  (-8,"3x","-46x + 56") ~=? showStep (divisionStep p q),
  (3,"-x","11x^2 + 17x + 9") ~=? showStep (divisionStep r p),
  (-8,"-x^2","-14x^2 - 64x - 24") ~=? showStep (divisionStep r q),
  (-4,"5x^2","-10x^2") ~=? showStep (divisionStep p6 q),
  (10,"-1","20x^2 + 80x + 30") ~=? showStep (divisionStep r p6),
  (8,"5x^3","0") ~=? showStep (divisionStep p6 p7),
  (1,"0","0") ~=? showDivision (divide (Polynomial []) p3),
  (1,"0",printPolynomial p5) ~=? showDivision (divide p5 p4),
  (1,"1","2") ~=? showDivision (divide p1 p2),
  (1,"x - 1","-1") ~=? showDivision (divide p3 p1),
  (16,"x + 1","0") ~=? showDivision (divide p1 p7),
  (16,"x^3 + 3","0") ~=? showDivision (divide p4 p7),
  (9,"3x - 1","-8x + 8") ~=? showDivision (divide p8 p9),
  (4,"x + 2","0") ~=?
    showDivision (divide (parsePolynomial "2x+4") (parsePolynomial "8")),
  (8,"4x^4 + 2x^2 - 127","8x - 159") ~=?
    showDivision (divide (parsePolynomial "x^6-32x^2+x-4")
                  (parsePolynomial "2x^2-1")),
  1 ~=? content p1,
  1 ~=? content p4,
  10 ~=? content p6,
  16 ~=? content p7,
  2 ~=? content (parsePolynomial "8x^3-6x^2+16x-30"),
  p1 ~=? polynomialGcd p1 (p1*p2),
  p1 ~=? polynomialGcd (p1*p2) p1,
  p1*p2 ~=? polynomialGcd (p1^(3 :: Int) * p2) (p1 * p2^(5 :: Int)),
  p10^(2::Int)*p11^(2::Int) ~=?
    polynomialGcd (p10^(5::Int) * p11^(2::Int)) (p10^(2::Int) * p11^(3::Int)),
  () ~=? ()]
  where
    showStep (constant, quotient, remainder)
      = (constant, printPolynomial (Polynomial [quotient]),
         printPolynomial remainder)
    showDivision (constant, quotient, remainder)
      = (constant, printPolynomial quotient, printPolynomial remainder)
{-
p1 = parsePolynomial "x+1"
p2 = parsePolynomial "x-1"
p3 = parsePolynomial "x^2-2"
p4 = parsePolynomial "x^3+3"
p5 = parsePolynomial "x+5"
p6 = parsePolynomial "10x^3"
p7 = parsePolynomial "16"
p8 = parsePolynomial "x^3 - x^2 - x + 1"
p9 = parsePolynomial "3x^2 - 2x - 1"
p10 = parsePolynomial "2x-2"
p11 = parsePolynomial "3x+3"
-}
squareFreeTests :: Test
squareFreeTests = TestList [
  [("x + 1)",1)] ~=? printResults (squareFree $ p1),
  [("x + 1)",2)] ~=? printResults (squareFree $ p1*p1),
  [("x^2 - 1)",1)] ~=? printResults (squareFree $ p1*p2),
  [("x + 1",2),("x - 1",1)] ~=? printResults (squareFree $ p1*p1*p2),
  () ~=? ()]

tests :: Test
tests = TestList [polynomialTests, modularPolynomialTests, polynomialOpTests{-,
                  squareFreeTests-}]

runTests :: IO Counts
runTests = runTestTT tests
