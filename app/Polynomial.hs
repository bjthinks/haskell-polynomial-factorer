module Polynomial where

import Defs

newtype Polynomial = Polynomial [(Coeff,Exponent)]
  deriving Eq

instance Show Polynomial where
  show (Polynomial []) = "0"
  show (Polynomial [(c,0)]) = show c
  show (Polynomial [(c,1)]) = show c ++ "x"
  show (Polynomial [(c,e)]) = show c ++ "x^" ++ show e
