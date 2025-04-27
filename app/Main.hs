module Main where

import Tests

main :: IO ()
main = do
  summary <- runTests
  print summary