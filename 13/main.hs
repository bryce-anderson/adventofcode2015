import Data.Char

import Data.List (permutations)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

parseInput [] = []
parseInput (l:ls) = d : parseInput ls where
  [p,_,s,i,_,_,_,_,_,_,p2] = words l
  d = ((p, p2'), r)
  p2' = takeWhile isAlpha p2
  sgn = if s == "lose" then (-1) else 1
  r = sgn * (read i)

run1 m (p:ps) = run1' (p:ps) where
  run1' (p1:p2:ps) = (m M.! (p1,p2)) + (m M.! (p2,p1)) + run1' (p2:ps)
  run1' [p'] = (m M.! (p,p')) + (m M.! (p',p))

main = do
  putStrLn "Day 13: Happy Dinner Party"
  i <- (parseInput . lines) `fmap` readFile "input"
  -- part 1
  let imap = M.fromList i
      ns = S.toList $ S.fromList $ (fst . fst) `fmap` i
      happy1 = maximum $ run1 imap `fmap` permutations ns

  print happy1

  -- part 2
  let me = "me"
      ns' = me:ns
      imap' = foldl f imap ns where
        f :: M.Map (String,String) Integer -> String -> M.Map (String,String) Integer
        f acc s = M.insert (me,s) 0 (M.insert (s,me) 0 acc)
      happy2 = maximum $ run1 imap' `fmap` permutations ns'

  print happy2

