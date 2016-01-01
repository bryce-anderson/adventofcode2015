import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Control.Monad (forM_)
import qualified Control.Monad.State.Strict as ST

import Data.Char

type Fragment = [String]

fragments :: String -> Fragment
fragments [] = []
fragments [c] = [[c]]
fragments (a:b:cs) | isLower b = [a,b] : fragments cs
                   | otherwise = [a] : fragments (b:cs)

parseInput :: String -> (Fragment, M.Map String [Fragment])
parseInput = go M.empty . lines where
  go acc (l:ls) = result where
    insert k v = case M.lookup k acc of
                  Just lst -> M.insert k (v:lst) acc
                  Nothing -> M.insert k [v] acc

    result = case words l of
      [a,_,b] -> go (insert a (fragments b)) ls
      [] -> (fragments (head ls), acc)

-- this would be better using Vector
formulas frags formula = ST.execState (go 0) S.empty where
  len = length formula
  go i | i >= len = return ()
       | otherwise = do
          forM_ subs $ \ins ->
            ST.modify $ S.insert (h ++ ins ++ t)
          go (i+1)

    where
     h = take i formula
     t = drop (i+1) formula
     ch = formula !! i
     subs = case M.lookup ch frags of
              Just subs -> subs
              Nothing -> []

main = do
  putStrLn "Day 19: Sick Raindeer"
  (formula,subs) <- parseInput `fmap` readFile "input"
  -- print subs
  -- print formula
  print $ S.size $ formulas subs formula

