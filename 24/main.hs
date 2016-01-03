{-# LANGUAGE TemplateHaskell #-}

import Control.Lens

import Control.Monad.State.Strict

import Control.Applicative

import Data.Foldable (foldr)
import Data.List (sort, tails)

import qualified Data.Sequence as Sq

import Prelude hiding (foldr)

data Calc = Calc { acc :: Int
                 , presents :: Sq.Seq Int
                 , remaining :: [Int] }
      deriving (Show)

data CalcState = CalcState { _minPresents :: Int
                           , _queue :: Sq.Seq Calc
                           , _complete :: [Calc] }
      deriving (Show)
makeLenses ''CalcState

findMinCombo :: Int -> [Int] -> [Calc]
findMinCombo target allPresents = result where
  result = _complete $ execState loop initial
  initial = CalcState maxBound (Sq.singleton (Calc 0 Sq.empty allPresents)) []

  runElement (Calc acc ps rem) = do
    let pstails = tails rem
    forM_ pstails $ \tl -> case tl of
      [] -> return () -- no more presents to add
      (x:xs) ->
        if n < target then do
          queue %= (Sq.|> next)

        else if n == target then do
          minPresents .= Sq.length (presents next)
          complete %= (next :)

        else return () -- too large and the tail will already be handled

        where
          n = acc + x
          next = Calc n (ps Sq.|> x) xs


  loop = do
    (CalcState minP q _) <- get
    if Sq.null q then return ()
    else let calc = Sq.index q 0 in do
      queue %= (Sq.drop 1)
      if minP <= Sq.length (presents calc) then loop -- not the min
      else runElement calc >> loop

minEntanglement :: [Calc] -> Int
minEntanglement = minimum . fmap f where
  f (Calc _ ps _) = foldr (*) 1 ps

main = do
  putStrLn "Day 24: Balance the Sleigh"
  nums <- (sort . fmap read . lines) <$> readFile "input"

  let total = sum nums
  putStrLn $ "All presents: " ++ show nums

  putStrLn "Part 1"
  print $ minEntanglement $ findMinCombo (total `div` 3) nums

  putStrLn "Part 2"
  print $ minEntanglement $ findMinCombo (total `div` 4) nums
