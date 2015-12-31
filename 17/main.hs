import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

amount = 150

combos :: [Int] -> Int
combos sizes = go sizes init where
  init = V.create (do { v <- MV.replicate (amount+1) 0; MV.write v 0 1; return v })
  go [] acc = acc V.! amount
  go (inc:is) acc = go is $ V.imap f acc where
    f i v | i < inc = v
          | otherwise = v + (acc V.! (i-inc))

-- represents the capacity and the containers used
data Path = Path { count :: Int
                 , numContainers :: Int }
        deriving (Eq, Ord, Show)

minCombos :: [Int] -> Path
minCombos sizes = go sizes init where
  init = V.create $ do
           v <- MV.replicate (amount+1) (Path maxBound maxBound);
           MV.write v 0 (Path 1 0)
           return v

  go [] acc = acc V.! amount
  go (inc:is) acc = go is $ V.imap f acc where
    f i v@(Path c n) | i < inc = v
                     | n' >= n = v                -- already a faster path
                     | n' + 1 == n = Path (c+c') n -- same distance
                     | otherwise = Path c' (n'+1) -- new faster path
      where
        Path c' n' = acc V.! (i-inc)


main = do
  putStrLn "Day 17: Too much Nog."
  sizes <- (fmap read . lines) `fmap` readFile "input"

  putStr "Part 1- all Combos: "
  print $ combos sizes

  putStr "Part 2- mincombos: "
  print $ minCombos sizes
