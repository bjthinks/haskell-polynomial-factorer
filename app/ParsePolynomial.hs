module ParsePolynomial(parsePolynomial) where

import Text.Parsec
--import Text.Parsec.Char
import Control.Applicative (some)
import Defs
import Polynomial

type MyParser = Parsec String ()

infixl 3 |||
(|||) :: MyParser a -> MyParser a -> MyParser a
(|||) lhs rhs = try lhs <|> rhs

parsePolynomial :: String -> Polynomial
parsePolynomial str =
  let result = parse pPolynomial "" str
  in case result of
    Left err -> error $ show err
    Right poly -> poly

pSign :: MyParser Char
pSign = oneOf "+-"

pCoeff :: MyParser Coeff
pCoeff = do
  number <- some digit
  return (read number :: Coeff)

pX :: MyParser ()
pX = char 'x' >> return ()

pCarat :: MyParser ()
pCarat = char '^' >> return ()

pExponent :: MyParser Exponent
pExponent = do
  number <- some digit
  return (read number :: Exponent)

pGeneralTerm :: MyParser Term
pGeneralTerm = do
  c <- pCoeff
  pX
  pCarat
  e <- pExponent
  return (c,e)

pLinearTerm :: MyParser Term
pLinearTerm = do
  c <- pCoeff
  pX
  return (c,1)

pTerm :: MyParser Term
pTerm = pGeneralTerm ||| pLinearTerm

pSignedTerm :: MyParser Term
pSignedTerm = do
  s <- pSign
  (c,e) <- pTerm
  let sc = if s == '+' then c else -c
  return (sc,e)

pLeadingTerm :: MyParser Term
pLeadingTerm = pSignedTerm ||| pTerm

pPolynomial :: MyParser Polynomial
pPolynomial = do
  t <- pLeadingTerm
  return $ Polynomial [t]
