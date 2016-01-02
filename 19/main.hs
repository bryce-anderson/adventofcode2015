{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Debug.Trace

import Data.Monoid

import qualified Data.ByteString.Char8 as BS

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Sequence as Sq

import Control.Applicative
import Control.Monad (forM_)
import qualified Control.Monad.State.Strict as ST

import Data.Char

type Fragment = [ BS.ByteString ]
type Substitutions = M.Map BS.ByteString [Fragment]

fragments :: String -> Fragment
fragments [] = []
fragments [c] = [ BS.singleton c ]
fragments (a:b:cs) | isLower b = (BS.pack [a,b]) : fragments cs
                     | otherwise = (BS.singleton a) : fragments (b:cs)

parseInput :: String -> (Fragment, Substitutions)
parseInput = go M.empty . lines where
  go acc (l:ls) = result where
    insert k v = case M.lookup k acc of
                  Just lst -> M.insert k (v:lst) acc
                  Nothing -> M.insert k [v] acc

    result = case words l of
      [a,_,b] -> go (insert (BS.pack a) (fragments b)) ls
      [] -> (fragments (head ls), acc)

substitute :: Substitutions -> Fragment -> [Fragment]
substitute frags formula = go 0 where
  len = length formula
  go i | i >= len = []
       | otherwise = (fmap (\m -> h <> m <> t) subs) ++ go (i+1)
   where
    h = take i formula
    ch = formula !! i
    t = drop (i+1) formula
    subs = case M.lookup ch frags of
             Just subs -> subs
             Nothing -> []

formulas frags formula = S.fromList $ substitute frags formula

main = do
  putStrLn "Day 19: Sick Raindeer"
  (formula,subs) <- parseInput `fmap` readFile "input"

  putStrLn $ "Tokens: " ++ show (length formula)

  putStr "Part 1: "
  print $ S.size $ formulas subs formula

  putStr "Part 2: "

