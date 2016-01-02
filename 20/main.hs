import qualified Data.Set as S

import Data.Foldable
import Prelude hiding (sum)

primesToGT m = sieve [2..m]
  where
    sieve (p:xs)
      | p*p > m = p : xs
      | True    = p : sieve [x | x <- xs, rem x p > 0]

primes = primesToGT 100000000

factorize :: Integer -> [Integer]
factorize i = go i primes where
  go i (p:ps)
    | p > i = []
    | m == 0 = p : go d (p:ps)
    | otherwise = go i ps
    where
      (d,m) = divMod i p

devisors :: Integer -> S.Set Integer
devisors i = S.fromList (1 : go fs) where
  fs = factorize i
  go [] =[]
  go (p:ps) = p : ((*p) `fmap` r) ++ r
    where r = go ps

magicNumber = 3400000 -- 3400000

firstNumber = go 1 where
  go i
    | s >= magicNumber = i
    | otherwise = go (i+1)
   where
     divs = devisors i
     s = sum divs

firstNumber2 = go 1 where
  go i
    | 11*s >= 10*magicNumber = i
    | otherwise = go (i+1)
   where
     divs = devisors i
     s = sum $ filter (\d -> d*50 >= i) $ S.toList divs


main = do
  putStrLn "Day 20: Presents"

  print $ devisors 11

  -- putStr "Part 1: "
  -- print $ firstNumber

  putStr "Part 2: "
  print $ firstNumber2
