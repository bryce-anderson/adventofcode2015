
final ('(':cs) acc = final cs (acc+1)
final (')':cs) acc = final cs (acc-1)
final [] acc = acc

first ('(':cs) acc c = first cs (acc+1) (c+1)
first (')':cs) 0   c = c+1
first (')':cs) acc c = first cs (acc-1) (c+1)
first [] _ _ = error "Never went into the basement..."


main = do
  putStrLn "Hello world!"
  i <- readFile "input"
  print $ final i 0
  print $ first i 0 0

