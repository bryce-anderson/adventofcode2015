import Data.List (isInfixOf, find)
import Data.Maybe (isJust)


vouls = "aeiou"
invalid = ["ab", "cd", "pq", "xy"]

firstIsNice str = (not illegalSubstr) && threeVouls && (pairs str) where
  threeVouls = 3 == (length . take 3 $ filter (\c -> elem c vouls) str)

  illegalSubstr = isJust $ find (\i -> isInfixOf i str) invalid

  pairs (c1:c2:cs) | c1 == c2 = True
                   | otherwise = pairs (c2:cs)
  pairs _                    = False

secondIsNice :: String -> Bool
secondIsNice str = pairs str && repeated str where
  pairs (a:b:rs) | isInfixOf [a,b] rs = True
                 | otherwise = pairs (b:rs)
  pairs _ = False

  repeated (x1:s:x2:rs) | x1 == x2 = True
                        | otherwise = repeated (s:x2:rs)
  repeated _ = False

main = do
  putStrLn "Nice strings"
  strs <- lines `fmap` readFile "input"
  putStrLn $ "First rules nice strings: " ++ show (length $ filter firstIsNice strs)
  putStrLn $ "New rules nice strings: " ++ show (length $ filter secondIsNice strs)

