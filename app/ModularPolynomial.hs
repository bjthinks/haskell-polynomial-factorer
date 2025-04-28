module ModularPolynomial where

import Data.List
import Defs
import Polynomial

data ModularPolynomial =
  ModularPolynomial { modulus :: Coeff, terms :: [Term] }

printModularPolynomial :: ModularPolynomial -> String
printModularPolynomial (ModularPolynomial m ts) =
  printPolynomial (Polynomial ts) ++ " mod " ++ show m

makeModularPolynomial :: Coeff -> [Term] -> ModularPolynomial
makeModularPolynomial m =
  ModularPolynomial m . eliminateZeros . addLikeTerms . sortTerms
  where
    eliminateZeros [] = []
    eliminateZeros ((0,_):ts) = ts
    eliminateZeros (t:ts) = t : eliminateZeros ts
    addLikeTerms [] = []
    addLikeTerms [t] = [t]
    addLikeTerms (t1@(c1,e1):t2s@((c2,e2):ts))
      | e1 == e2 = addLikeTerms ((c1+c2 `mod` m,e1):ts)
      | otherwise = t1 : addLikeTerms t2s
    sortTerms = reverse . sortOn snd
