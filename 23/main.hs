import Control.Applicative
import Control.Monad

import Debug.Trace

import qualified Data.Vector as V

data Reg = RA | RB
  deriving (Show, Eq)

data Inst = Hlf Reg
          | Tpl Reg
          | Inc Reg
          | Jmp Int
          | Jie Reg Int
          | Jio Reg Int
      deriving (Show, Eq)

type Program = V.Vector Inst

parseLine :: String -> Inst
parseLine s = case words (filter f s) of
  ["hlf", r] -> Hlf (p r)
  ["tpl", r] -> Tpl (p r)
  ["inc", r] -> Inc (p r)
  ["jmp", o] -> Jmp (read o)
  ["jie", r, o] -> Jie (p r) (read o)
  ["jio", r, o] -> Jio (p r) (read o)
  where
    p "a" = RA
    p "b" = RB

    f '+' = False
    f ',' = False
    f  _  = True

data St = St { _a :: Int, _b :: Int }
  deriving (Show)

interpret :: Program -> St -> St
interpret p s = go 0 s where
  l = V.length p

  go :: Int -> St -> St
  go i st | i == l = st
  go i s@(St a b) = -- trace ((show i) ++ (' ':(show s))) $
    case p V.! i of
      Hlf RA -> go (i+1) (St (a `div` 2) b)
      Hlf RB -> go (i+1) (St a (b `div` 2))

      Tpl RA -> go (i+1) (St (a*3) b)
      Tpl RB -> go (i+1) (St a (b*3))

      Inc RA -> go (i+1) (St (a+1) b)
      Inc RB -> go (i+1) (St a (b+1))

      Jmp j  -> go (i+j) s

      Jie RA j | even a    -> go (i+j) s
               | otherwise -> go (i+1) s

      Jie RB j | even b    -> go (i+j) s
               | otherwise -> go (i+1) s

      Jio RA j | a == 1     -> go (i+j) s
               | otherwise -> go (i+1) s

      Jio RB j | b == 1     -> go (i+j) s
               | otherwise -> go (i+1) s


main = do
  putStrLn "Day 23: Turning Lock"
  instrs <- (fmap parseLine . lines) <$> readFile "input"

  forM_ ([0..] `zip` instrs) print

  putStr "Part 1: "
  print $ interpret (V.fromList instrs) (St 0 0)

  putStr "Part 2: "
  print $ interpret (V.fromList instrs) (St 1 0)

