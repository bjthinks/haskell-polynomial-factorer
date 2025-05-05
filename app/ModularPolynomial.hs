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
    eliminateZeros ((Term 0 _):ts) = ts
    eliminateZeros (t:ts) = t : eliminateZeros ts
    addLikeTerms [] = []
    addLikeTerms [t] = [canonical t]
    addLikeTerms (t1@(Term c1 e1):t2s@((Term c2 e2):ts))
      | e1 == e2 = addLikeTerms ((Term ((c1+c2) `mod` m) e1):ts)
      | otherwise = canonical t1 : addLikeTerms t2s
    canonical (Term c e) = Term (c `mod` m) e
    sortTerms = reverse . sortOn termExponent

-- NOTE: This function does not check if c1*c2 == 0 mod m!
mMultiplyTermByTerm :: Coeff -> Term -> Term -> Term
mMultiplyTermByTerm m (Term c1 e1) (Term c2 e2) = Term ((c1*c2) `mod` m) (e1+e2)

mMultiplyTermByList :: Coeff -> Term -> [Term] -> [Term]
mMultiplyTermByList m t@(Term c _)
  | c `mod` m == 0 = const []
  | otherwise = map $ mMultiplyTermByTerm m t

mMultiplyTermByPolynomial :: Term -> ModularPolynomial -> ModularPolynomial
mMultiplyTermByPolynomial t (ModularPolynomial m ts) =
  ModularPolynomial m (mMultiplyTermByList m t ts)

mMultiplyConstantByPolynomial :: Coeff -> ModularPolynomial -> ModularPolynomial
mMultiplyConstantByPolynomial c f = mMultiplyTermByPolynomial (Term c 0) f

instance Num ModularPolynomial where
  ModularPolynomial m1 xs + ModularPolynomial m2 ys
    | m1 /= m2 = error "Incompatible modulus in +"
    | otherwise = ModularPolynomial m1 $ addTerms xs ys
    where
      addTerms ps [] = ps
      addTerms [] qs = qs
      addTerms allps@(t1@(Term c1 e1):ps) allqs@(t2@(Term c2 e2):qs)
        | e1 > e2 = t1 : addTerms ps allqs
        | e1 < e2 = t2 : addTerms allps qs
        | (c1+c2) `mod` m1 == 0 = addTerms ps qs
        | otherwise = Term ((c1+c2) `mod` m1) e1 : addTerms ps qs
  -- * could be made more efficient
  -- for instance, join together like terms before collecting all possible
  -- cross terms into one giant list...
  ModularPolynomial m1 xs * ModularPolynomial m2 ys
    | m1 /= m2 = error "Incompatible modulus in *"
    | otherwise = makeModularPolynomial m1 $ multiplyTerms xs ys
    where
      multiplyTerms [] _ = []
      multiplyTerms (p:ps) qs = mMultiplyTermByList m1 p qs ++
        multiplyTerms ps qs
  negate (ModularPolynomial m xs) = ModularPolynomial m $ negateTerms xs
    where
      negateTerms [] = []
      negateTerms (Term c e : ts) = Term ((-c) `mod` m) e : negateTerms ts
  abs _ = error "No abs for ModularPolynomial"
  signum _ = error "No signum for ModularPolynomial"
  fromInteger _ = error "No fromInteger for ModularPolynomial"

mDerivative :: ModularPolynomial -> ModularPolynomial
mDerivative (ModularPolynomial m ts) = ModularPolynomial m (applyDiff ts)
  where
    applyDiff [] = []
    applyDiff [Term _ 0] = []
    applyDiff (Term c e : us)
      | e `mod` m == 0 = applyDiff us
      | otherwise = Term ((c*e) `mod` m) (e-1) : applyDiff us

mLeadingTerm :: ModularPolynomial -> Term
mLeadingTerm (ModularPolynomial _ []) =
  error "zero polynomial has no leading term"
mLeadingTerm (ModularPolynomial _ ts) = head ts

mLeadingCoeff :: ModularPolynomial -> Coeff
mLeadingCoeff = termCoeff . mLeadingTerm

mDegree :: ModularPolynomial -> Exponent
mDegree = termExponent . mLeadingTerm

mConstantPolynomial :: Coeff -> Coeff -> ModularPolynomial
mConstantPolynomial m c
  | c `mod` m == 0 = ModularPolynomial m []
  | otherwise = ModularPolynomial m [Term (c `mod` m) 0]

invertMod :: Coeff -> Coeff -> Coeff
invertMod m c =
  let (g,_,b) = extendedGcd m c
  in if g /= 1
     then error ("No modular inverse of " ++ show c ++ " mod " ++ show m)
     else b

extendedGcd :: Coeff -> Coeff -> (Coeff, Coeff, Coeff)
extendedGcd x 0 = (x,1,0)
extendedGcd x y =
  let q = x `div` y
      r = x `mod` y
      (g,a,b) = extendedGcd y r
  in (g,b,a-b*q)

mDivisionStep :: ModularPolynomial -> ModularPolynomial ->
  (Term, ModularPolynomial)
mDivisionStep _ (ModularPolynomial _ []) = error
  "attempt to divide a modular polynomial by 0"
mDivisionStep (ModularPolynomial _ []) _ = error
  "no terms to divide in mDivisionStep"
mDivisionStep dividend divisor =
  let m = modulus dividend
      Term dividendLeadingCoeff dividendDegree = mLeadingTerm dividend
      Term divisorLeadingCoeff divisorDegree = mLeadingTerm divisor
      quotientCoeff = (dividendLeadingCoeff *
                       invertMod m divisorLeadingCoeff) `mod` m
      quotientExponent = dividendDegree - divisorDegree
      quotientTerm = Term quotientCoeff quotientExponent
      remainderPoly = dividend - mMultiplyTermByPolynomial quotientTerm divisor
  in (quotientTerm, remainderPoly)

mDivide :: ModularPolynomial -> ModularPolynomial ->
  (ModularPolynomial, ModularPolynomial)
mDivide startingDividend divisor =
  let (q, r) = divide' startingDividend
  in (ModularPolynomial m q, r)
  where
    m1 = modulus startingDividend
    m2 = modulus divisor
    m = if m1 == m2 then m1 else error "incompatible moduluses in mDivide"
    divide' :: ModularPolynomial -> ([Term], ModularPolynomial)
    divide' dividend
      | terms divisor == [] = error
        "attempt to divide a modular polynomial by zero"
      | terms dividend == [] = ([], ModularPolynomial m [])
      | mDegree dividend < mDegree divisor = ([], dividend)
      | otherwise =
        let (q1, r1) = mDivisionStep dividend divisor
            (q2, r2) = divide' r1
        in (q1 : q2, r2)
