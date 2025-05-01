module SquareFree(squareFree) where

import System.IO.Unsafe
import Defs
import Polynomial

squareFree :: Polynomial -> [(Polynomial, Exponent)]
squareFree f =
  let f' = derivative f
      a0 = polynomialGcd f f'
      b1 = divide' f a0
      c1 = divide' f' a0
      d1 = c1 - derivative b1
      in unsafePerformIO (print (printPolynomial f') >> print (printPolynomial a0) >> print (printPolynomial b1) >> print (printPolynomial c1) >> print (printPolynomial d1)) `seq` yun b1 d1 1
  where
    yun (Polynomial [Term _ 0]) _ _ = []
    yun bi di i = unsafePerformIO (print (printPolynomial bi) >> print (printPolynomial di) >> print i) `seq`
      let ai = polynomialGcd bi di
          bip = divide' bi ai
          cip = divide' di ai
          dip = cip - derivative bip
          rest = yun bip dip (i+1)
      in if isConstant ai then rest else (ai,i) : rest
    divide' x y = (\(_,z,_) -> z) (divide x y)
    isConstant (Polynomial []) = True
    isConstant (Polynomial [Term _ 0]) = True
    isConstant _ = False
