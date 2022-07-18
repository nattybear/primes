import Data.Maybe
import Primes
import Test.QuickCheck

prop_validPrimesOnly val = if val < 0 || val >= length primes
                           then result == Nothing
                           else isJust result
  where result = isPrime val

prop_primesArePrime val = if result == Just True
                          then length divisors == 0
                          else True
  where result   = isPrime val
        divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

main :: IO ()
main = do
  quickCheck prop_validPrimesOnly
  quickCheck prop_primesArePrime
