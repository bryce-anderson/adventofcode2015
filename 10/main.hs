import Data.Foldable (for_)

import Data.STRef
import Control.Monad.ST

import Data.Char

input = digitToInt `fmap` "1113122113"

forward []     = []
forward (d:ds) = go 1 ds where
  go acc (n:ds) | d == n = go (acc+1) ds
                | otherwise = acc:d:forward (n:ds)
  go acc [] = [acc,d]

main = do
  putStrLn "Day 10: look-and-day"

  print input

  let result times = runST $ do
      acc <- newSTRef input
      for_ [1..times] $ \_ ->
        modifySTRef acc forward
      readSTRef acc

  putStr "Going 40 times: "
  print $ length $ result 40

  putStr "Going 50 times: "
  print $ length $ result 50

