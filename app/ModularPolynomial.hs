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
    addLikeTerms [t] = [canonical t]
    addLikeTerms (t1@(c1,e1):t2s@((c2,e2):ts))
      | e1 == e2 = addLikeTerms (((c1+c2) `mod` m,e1):ts)
      | otherwise = canonical t1 : addLikeTerms t2s
    canonical (c,e) = (c `mod` m,e)
    sortTerms = reverse . sortOn snd

instance Num ModularPolynomial where
  ModularPolynomial m1 xs + ModularPolynomial m2 ys
    | m1 /= m2 = error "Incompatible modulus in +"
    | otherwise = ModularPolynomial m1 $ addTerms xs ys
    where
      addTerms ps [] = ps
      addTerms [] qs = qs
      addTerms allps@(t1@(c1,e1):ps) allqs@(t2@(c2,e2):qs)
        | e1 > e2 = t1 : addTerms ps allqs
        | e1 < e2 = t2 : addTerms allps qs
        | (c1+c2) `mod` m1 == 0 = addTerms ps qs
        | otherwise = ((c1+c2) `mod` m1,e1) : addTerms ps qs
  -- * could be made more efficient
  -- for instance, join together like terms before collecting all possible
  -- cross terms into one giant list...
  ModularPolynomial m1 xs * ModularPolynomial m2 ys
    | m1 /= m2 = error "Incompatible modulus in *"
    | otherwise = makeModularPolynomial m1 $ multiplyTerms xs ys
    where
      multiplyTerms [] _ = []
      multiplyTerms (p:ps) qs = multiplyTerm p qs ++ multiplyTerms ps qs
      multiplyTerm _ [] = []
      multiplyTerm t@(c1,e1) ((c2,e2):ts) =
        ((c1*c2) `mod` m1,e1+e2) : multiplyTerm t ts
  negate (ModularPolynomial m xs) = ModularPolynomial m $ negateTerms xs
    where
      negateTerms [] = []
      negateTerms ((c,e):ts) = ((-c) `mod` m,e) : negateTerms ts
  abs _ = error "No abs for ModularPolynomial"
  signum _ = error "No signum for ModularPolynomial"
  fromInteger _ = error "No fromInteger for ModularPolynomial"
