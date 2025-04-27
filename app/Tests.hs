module Tests where

import Test.HUnit
import Polynomial

testEqual :: (Eq a, Show a) => String -> a -> a -> Test
testEqual name x y = TestCase $ assertEqual name x y

testTrue :: String -> Bool -> Test
testTrue name b = TestCase $ assertBool name b

polynomialTests :: Test
polynomialTests = TestList [testEqual "First test" (1+1) (2 :: Int)]

test2 :: Test
test2 = testTrue "Second test" $ 2*2 == (4 :: Int)

tests :: Test
tests = TestList [polynomialTests, test2]

runTests :: IO Counts
runTests = runTestTT tests
