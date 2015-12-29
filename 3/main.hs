import qualified Data.Set as S


data State = State Integer Integer
  deriving (Eq, Ord, Show)

trans '>' (State x y) = State (x+1) y
trans '<' (State x y) = State (x-1) y
trans '^' (State x y) = State x (y+1)
trans 'v' (State x y) = State x (y-1)

part1 (st1, acc) ins = (st2, acc2) where
  st2 = trans ins st1
  acc2 = S.insert st2 acc

part2 (s1, s2, acc) ins = (s2, s1t, accn) where
  s1t = trans ins s1
  accn = S.insert s1t acc



main = do
  putStrLn "Santa directions"
  i <- readFile "input"
  let (_, fs1) = foldl part1 (z,s) i
      z = State 0 0
      s = S.singleton z

  print $ S.size fs1

  let (_,_,fs2) = foldl part2 (z,z,s) i

  print $ S.size fs2

