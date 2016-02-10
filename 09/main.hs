import Data.Foldable
import Data.List (permutations)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Prelude hiding (foldl, foldr, minimum, maximum, concat)

data Dist = Dist { from :: String
                 , to :: String
                 , dist ::Int }
  deriving (Show, Eq)

parseLine str = Dist from to len where
  [from,_,to,_,lstr] = words str
  len = read lstr


main = do
  putStrLn "Day 9: the shortest route"
  dists <- (fmap parseLine . lines) `fmap` readFile "input"

  let distMap = foldl f Map.empty dists where
        f acc (Dist f t d) = Map.insert (f,t) d $ Map.insert (t,f) d acc

      distSet = Set.fromList $ concat ((\d -> [from d, to d]) `fmap` dists)
      distPermutations = permutations $ Set.elems distSet

      distance (a:b:ds) = (distMap Map.! (a,b)) + distance (b:ds)
      distance _        = 0

      totalDists = distance `fmap` distPermutations

  putStr "Minimum distance: "
  print $ minimum totalDists

  putStr "Maximum distance: "
  print $ maximum totalDists



