module Tests where

import Test.HUnit

test1 = TestCase $ assertEqual "First test" 2 (1+1)
test2 = TestCase $ assertBool "Second test" $ 2*2 == 4

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

runTests = runTestTT tests
