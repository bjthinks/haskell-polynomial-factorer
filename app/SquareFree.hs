module SquareFree(squareFree) where

import Defs
import Polynomial

squareFree :: Polynomial -> [(Polynomial, Exponent)]
squareFree f =
  -- if f has content c, then f' has content ck for some k.
  -- then a0 has content c as well.
  -- b1 has no content
  -- so all bi have no content
  -- so all ai i>0 have no content
  let f' = derivative f
      a0 = polynomialGcd f f'
      b1 = divide' f a0
      c1 = divide' f' a0
      d1 = c1 - derivative b1
      in reverse (yun b1 d1 1)
  where
    yun (Polynomial [Term _ 0]) _ _ = []
    yun bi di i =
      let ai = polynomialGcd bi di
          bip = divide' bi ai
          cip = divide' di ai
          dip = cip - derivative bip
          rest = yun bip dip (i+1)
      in if isConstant ai then rest else (ai,i) : rest
    divide' x y = let (_,z,_) = divide x y in z
    isConstant (Polynomial []) = True
    isConstant (Polynomial [Term _ 0]) = True
    isConstant _ = False
