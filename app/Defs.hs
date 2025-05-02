module Defs where

type Coeff = Int
type Exponent = Int
data Term = Term { termCoeff :: Coeff, termExponent :: Exponent }
  deriving (Eq, Show, Read)
data (Eq p, Show p, Read p) =>
  Factorization p = Factorization Coeff [(p,Exponent)]
  deriving (Eq, Show, Read)
