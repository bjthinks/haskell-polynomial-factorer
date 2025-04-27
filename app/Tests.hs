module Tests where

import Test.HUnit

test1 :: Test
test1 = TestCase $ assertEqual "First test" (1+1) (2 :: Int)

test2 :: Test
test2 = TestCase $ assertBool "Second test" $ 2*2 == (4 :: Int)

tests :: Test
tests = TestList [test1, test2]

runTests :: IO Counts
runTests = runTestTT tests
