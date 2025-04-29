module Polynomial where

import Data.List
import Defs

newtype Polynomial = Polynomial [Term]
  deriving (Eq, Show, Read)

printPolynomial :: Polynomial -> String
printPolynomial (Polynomial []) = "0"
printPolynomial (Polynomial [t]) = prettyPrintTerm t
  where
    prettyPrintTerm (c,0) = show c
    prettyPrintTerm (1,e) = prettyPrintPower e
    prettyPrintTerm (-1,e) = "-" ++ prettyPrintPower e
    prettyPrintTerm (c,e) = show c ++ prettyPrintPower e
    prettyPrintPower 1 = "x"
    prettyPrintPower e = "x^" ++ show e
printPolynomial (Polynomial (t:ts)) =
  printPolynomial (Polynomial [t]) ++
  addSignAndSpaces (printPolynomial (Polynomial ts))
  where
    addSignAndSpaces ('-':str) = " - " ++ str
    addSignAndSpaces str = " + " ++ str

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
  Polynomial xs + Polynomial ys = Polynomial $ addTerms xs ys
    where
      addTerms ps [] = ps
      addTerms [] qs = qs
      addTerms allps@(t1@(c1,e1):ps) allqs@(t2@(c2,e2):qs)
        | e1 > e2 = t1 : addTerms ps allqs
        | e1 < e2 = t2 : addTerms allps qs
        | c1+c2 == 0 = addTerms ps qs
        | otherwise = (c1+c2,e1) : addTerms ps qs
  -- * could be made more efficient
  -- for instance, join together like terms before collecting all possible
  -- cross terms into one giant list...
  Polynomial xs * Polynomial ys = makePolynomial $ multiplyTerms xs ys
    where
      multiplyTerms [] _ = []
      multiplyTerms (p:ps) qs = multiplyTerm p qs ++ multiplyTerms ps qs
      multiplyTerm _ [] = []
      multiplyTerm t@(c1,e1) ((c2,e2):ts) = (c1*c2,e1+e2) : multiplyTerm t ts
  negate (Polynomial xs) = Polynomial $ negateTerms xs
    where
      negateTerms [] = []
      negateTerms ((c,e):ts) = (-c,e) : negateTerms ts
  abs _ = error "No abs for Polynomial"
  signum _ = error "No signum for Polynomial"
  fromInteger c = makePolynomial [(fromInteger c,0)]

derivative :: Polynomial -> Polynomial
derivative p@(Polynomial []) = p
