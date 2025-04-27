module Tests where

import Test.HUnit
import Polynomial

testTrue :: String -> Bool -> Test
testTrue name b = TestCase $ assertBool name b

polynomialTests :: Test
polynomialTests = TestList [
  "First test" ~: 1+1 ~=? (2 :: Int)
  ]

test2 :: Test
test2 = testTrue "Second test" $ 2*2 == (4 :: Int)

tests :: Test
tests = TestList [polynomialTests, test2]

runTests :: IO Counts
runTests = runTestTT tests
