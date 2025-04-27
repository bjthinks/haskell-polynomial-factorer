module Tests where

import Test.HUnit
import Polynomial

polynomialTests :: Test
polynomialTests = TestList [
  "First test" ~: 1+1 ~=? (2 :: Int)
  ]

tests :: Test
tests = TestList [polynomialTests]

runTests :: IO Counts
runTests = runTestTT tests
