import Data.Array.ST
import Data.Array.MArray
import Control.Monad.ST

import Data.List.Split (splitOn)
import Data.Foldable

type Coord = (Int,Int)

type ArrTpe s i = STUArray s Coord i

bs :: ((Int,Int),(Int,Int))
bs = ((0,0), (999,999))

data Mode = Toggle | On | Off
data Instr = Instr Mode Coord Coord

modMat1 :: ArrTpe s Bool -> Instr -> ST s ()
modMat1 mat (Instr m (x1,y1) (x2,y2)) =
    forM_ [x1..x2] $ \x ->
      forM_ [y1..y2] $ \y ->
        case m of
           On  -> writeArray mat (x,y) True
           Off -> writeArray mat (x,y) False
           Toggle -> do
             old <- readArray mat (x,y)
             writeArray mat (x,y) (not old)

modMat2 :: ArrTpe s Int -> Instr -> ST s ()
modMat2 mat (Instr m (x1,y1) (x2,y2)) =
    forM_ [x1..x2] $ \x ->
      forM_ [y1..y2] $ \y -> do
        old <- readArray mat (x,y)
        case m of
           On  -> writeArray mat (x,y) (old+1)
           Toggle -> writeArray mat (x,y) (old+2)
           Off -> writeArray mat (x,y) (if old <= 0 then 0 else old-1)

runInstrs1 :: [Instr] -> Integer
runInstrs1 instrs = runST $ do
  mat <- newArray bs False
  forM_ instrs $ modMat1 mat
  -- see how many are on
  es <- getElems mat
  let p :: Integer -> Bool -> Integer
      p acc f = if f then (acc+1) else acc
  let count = foldl' p 0 es
  return count

runInstrs2 :: [Instr] -> Integer
runInstrs2 instrs = runST $ do
  mat <- newArray bs 0
  forM_ instrs $ modMat2 mat
  -- see how many are on
  es <- getElems mat
  let f :: Integer -> Int -> Integer
      f acc i = acc + (fromIntegral i)
      count = foldl' f 0 es
  return count


makeInstr l = i where
  ws = case words l of
            ("turn":ws) -> ws
            ws          -> ws
  i = case ws of
       ["off",a,_,b]    -> Instr Off (coor a) (coor b)
       ["toggle",a,_,b] -> Instr Toggle (coor a) (coor b)
       ["on",a,_,b]     -> Instr On (coor a) (coor b)

  coor cs = let [a,b] = splitOn "," cs
            in (read a, read b)


main = do
  putStrLn "Grid of lights"
  i <- readFile "input"
  let instrs = makeInstr `fmap` lines i
      onLights1 = runInstrs1 instrs
      onLights2 = runInstrs2 instrs
  putStrLn $ "Lights on 1: " ++ show onLights1
  putStrLn $ "Lights on 2: " ++ show onLights2

