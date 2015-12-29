import Data.List.Split
import Data.List (sort)

-- gives an increasing set of numbers
parseLine :: String -> [Int]
parseLine l = sort $ read `fmap` splitOn "x" l

neededPaper [a,b,c] = 3*a*b + 2*a*c + 2*b*c

neededRibbon [a,b,c] = bow + wrap where
  bow = a*b*c
  wrap = 2*(a+b)

main = do
  putStrLn "The wrapping paper challenge."
  i <- readFile "input"
  let ls = parseLine `fmap` lines i
  print $ sum $ neededPaper `fmap` ls
  print $ sum $ neededRibbon `fmap` ls
