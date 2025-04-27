module Polynomial where

import Defs

newtype Polynomial = Polynomial [(Coeff,Exponent)]
  deriving Eq

instance Show Polynomial where
  show (Polynomial []) = "0"
  show (Polynomial [t]) = showTerm t
    where
      showTerm (c,0) = show c
      showTerm (1,e) = showPower e
      showTerm (-1,e) = "-" ++ showPower e
      showTerm (c,e) = show c ++ showPower e
      showPower 1 = "x"
      showPower e = "x^" ++ show e
