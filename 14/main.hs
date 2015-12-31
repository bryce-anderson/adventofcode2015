import qualified Data.Vector.Mutable as V
import qualified Data.Vector as V hiding (modify, replicate)
import Data.List (transpose)
import Data.Foldable (for_)
import Control.Monad.ST

data Deer = Deer { name :: String
                 , topSpeed :: Integer
                 , duration :: Integer
                 , rest :: Integer }
  deriving (Show, Eq, Ord)

parse l = deer where
  deer = Deer n (read s) (read d) (read r)
  [n,_,_,s,_,_,d,_,_,_,_,_,_,r,_] = words l

speed :: Deer -> [Integer]
speed (Deer _ s d r) = phase where
  phase = cycle $ replicate (fromIntegral d) s ++ replicate (fromIntegral r) 0

part2 deer time = V.toList $ runST $ do
  positions <- V.replicate (length deer) 0
  scores <- V.replicate (length deer) 0
  let ispeeds = transpose $ (take time . speed) `fmap` deer
  for_ ispeeds $ \speeds -> do
    for_ ([0..] `zip` speeds) $ \((i,s)) ->
      V.modify positions (+s) i

    cpos <- V.freeze positions
    let m = V.maximum cpos
        f i v = if v == m then V.modify scores (+1) i else return ()
    V.imapM_ f cpos

  V.freeze scores

main = do
  putStrLn "Day 14: Raindeer games"
  deer <- (fmap parse . lines) `fmap` readFile "input"

  print deer

  let time = 2503
  print $ maximum $ (sum . take time . speed) `fmap` deer
  print $ maximum $ part2 deer time

