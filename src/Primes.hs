module Primes where

primes :: [Int]
primes = sieve [2 .. 10000]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve noFactors
  where noFactors = filter (not . (== 0) . (`mod` nextPrime)) rest
