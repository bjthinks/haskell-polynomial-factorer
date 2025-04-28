module ParsePolynomial(parsePolynomial) where

import Text.Parsec
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

pPolynomial :: MyParser Polynomial
pPolynomial = undefined
