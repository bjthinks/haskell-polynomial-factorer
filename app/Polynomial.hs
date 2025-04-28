module Polynomial where

import Data.List
import Defs

newtype Polynomial = Polynomial [Term]
  deriving (Eq, Show, Read)

prettyPrint :: Polynomial -> String
prettyPrint (Polynomial []) = "0"
prettyPrint (Polynomial [t]) = prettyPrintTerm t
  where
    prettyPrintTerm (c,0) = show c
    prettyPrintTerm (1,e) = prettyPrintPower e
    prettyPrintTerm (-1,e) = "-" ++ prettyPrintPower e
    prettyPrintTerm (c,e) = show c ++ prettyPrintPower e
    prettyPrintPower 1 = "x"
    prettyPrintPower e = "x^" ++ show e
prettyPrint (Polynomial (t:ts)) =
  prettyPrint (Polynomial [t]) ++ addPlusSign (prettyPrint (Polynomial ts))
  where
    addPlusSign str@('-':_) = str
    addPlusSign str = "+" ++ str

makePolynomial :: [Term] -> Polynomial
makePolynomial = Polynomial . eliminateZeros . addLikeTerms . sortTerms
  where
    eliminateZeros [] = []
    eliminateZeros ((0,_):ts) = ts
    eliminateZeros (t:ts) = t : eliminateZeros ts
    addLikeTerms [] = []
    addLikeTerms [t] = [t]
    addLikeTerms (t1@(c1,e1):t2s@((c2,e2):ts))
      | e1 == e2 = addLikeTerms ((c1+c2,e1):ts)
      | otherwise = t1 : addLikeTerms t2s
    sortTerms = reverse . sortOn snd

instance Num Polynomial where
  _ + _ = undefined
  _ * _ = undefined
  negate _ = undefined
  abs _ = error "No abs for Polynomial"
  signum _ = error "No signum for Polynomial"
  fromInteger c = makePolynomial [(fromInteger c,0)]
