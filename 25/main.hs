
row = 2947
col = 3029

first = 20151125

-- go last row column
go :: Int -> Int -> Int
go row col = go' first 1 1 where
  go' last 0 c = go' last c 1
  go' last r c
    | r == row && c == col = last
    | otherwise = go' n (r-1) (c+1)
    where
    n = (last*252533) `rem` 33554393



main = do
  putStrLn "Day 25: Authorization Code"
  print $ go row col


