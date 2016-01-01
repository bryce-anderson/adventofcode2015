import Control.Monad (foldM, forM_)
import Control.Monad.ST
import Data.STRef

import Data.Array.ST
import Data.Array.MArray

type UPic s = STUArray s (Int,Int) Int

kernel :: UPic s -> (Int,Int) -> ST s Int
kernel arr (x,y) = do
  ((xmin,ymin),(xmax,ymax)) <- getBounds arr

  center <- readArray arr (x,y)
  acc <- newSTRef (-center)

  -- using forM_ and a [(x,y) | x <- [..], y <- [..]] works but is slower

  forM_ [max xmin (x-1) .. min xmax (x+1)] $ \x -> do
    forM_ [max ymin (y-1) .. min ymax (y+1)] $ \y -> do
      r <- readArray arr (x,y)
      modifySTRef acc (+r)

  readSTRef acc


runKernel :: UPic s -> UPic s -> ST s ()
runKernel old new = do
  ((xmin,ymin),(xmax,ymax)) <- getBounds old
  forM_ [ymin..ymax] $ \y -> do
    forM_ [xmin..xmax] $ \x -> do
      let xy = (x,y)
      i <- readArray old xy
      n <- kernel old xy
      let i' = if i == 1 then (if n == 2 || n == 3 then 1 else 0 )
             else (if n == 3 then 1 else 0)
      writeArray new xy i'

bounds = ((0,0),(99,99))

run :: (UPic s -> ST s ()) -> Int -> [Int] -> ST s Int
run mod iterations input = do
  a <- newListArray bounds input
  b <- newArray bounds 0
  final <- go a b iterations
  mod final
  vs <- getElems final
  return $ sum vs
 where
  go a b 0 = return a
  go a b i = mod a >> runKernel a b >> go b a (i-1)

parseData = fmap v . concat . lines
  where
    v '#' = 1
    v '.' = 0
    v o = error $ "Invalid character: " ++ show o

corners :: UPic s -> ST s ()
corners arr = do
  writeArray arr (0 , 0) 1
  writeArray arr (0 ,99) 1
  writeArray arr (99, 0) 1
  writeArray arr (99,99) 1

main = do
  putStrLn "Day 18: Like a GIF For Your Yard"
  input <- parseData `fmap` readFile "input"
  let c = runST $ run (\_ -> return ()) 100 input
  putStrLn $ "part 1: " ++ show c

  let c' = runST $ run corners 100 input
  putStrLn $ "part 2: " ++ show c'


