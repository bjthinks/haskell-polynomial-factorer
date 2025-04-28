module Polynomial where

import Data.List
import Text.Read
import Defs

newtype Polynomial = Polynomial [Term]
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
  show (Polynomial (t:ts)) =
    show (Polynomial [t]) ++ addPlusSign (show (Polynomial ts))
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

instance Read Polynomial where
  readPrec = undefined
  readListPrec = readListPrecDefault
