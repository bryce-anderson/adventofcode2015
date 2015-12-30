{-# LANGUAGE BangPatterns #-}

import Data.Char

maxChar = ord 'z'
minChar = ord 'a'

i = ord 'i'
o = ord 'o'
l = ord 'l'

-- doing (+) because the 'stack' will be in reverse
hasStraight (a:b:c:cs) = a == b+1 && b == c+1 || hasStraight (b:c:cs)
hasStraight _ = False

hasPairs = hasPairs' False where
  hasPairs' first (a:b:cs) | a == b = first || hasPairs' True cs
                           | otherwise = hasPairs' first (b:cs)
  hasPairs' _ _ = False


inc (d:ds) | dn == i = i+1:ds
           | dn == o = o+1:ds
           | dn == l = l+1:ds
           | dn > maxChar = minChar : inc ds
           | otherwise = dn:ds
  where
    !dn = d+1


start = "hepxcrrq"

encode :: String -> [Int]
encode = reverse . fmap ord

decode :: [Int] -> String
decode = reverse . fmap chr

next password = if hasStraight n && hasPairs n then n
                else next n where
  n = inc password


main = do
  putStrLn "Day 11: Santas Password"

  print start
  let n = next $ encode start
      nstr = decode n

  print nstr

  let n2 = next n
      nstr2 = decode n2

  print nstr2


