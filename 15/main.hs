import qualified Data.Vector as V

data Ing = Ing { name :: String
               , capacity :: Int
               , durability :: Int
               , flavor :: Int
               , texture :: Int
               , calories :: Int }
  deriving (Show, Eq, Ord)

type Recipe = [(Int,Ing)]

data Cookie = Cookie { totalCalories :: Int
                     , totalScore :: Int }
  deriving (Show, Eq, Ord)

maxIngredients = 100

score :: Recipe -> Cookie
score ingredients = Cookie cals total where
  property f = 0 `max` (sum $ (\((amount,ing)) -> amount * (f ing)) `fmap` ingredients)
  total = product $ property `fmap` [capacity, durability, flavor, texture]
  cals = sum $ (\((a,ing)) -> a * (calories ing)) `fmap` ingredients

parse line = Ing n c d f t cal where
  readInt = read . takeWhile (/= ',')
  [n',_,c',_,d', _,f',_,t',_,cal'] = words line
  n = takeWhile (/= ',') n'
  c = readInt c'
  d = readInt d'
  f = readInt f'
  t = readInt t'
  cal = read cal'

allRecipes :: [Ing] -> Int -> [Recipe]
allRecipes [ing] rem = [[(rem,ing)]]
allRecipes (ing:ings) rem = concat [go i | i <- [0.. rem]] where
  go i = ((i,ing):) `fmap` allRecipes ings (rem-i)


main = do
  putStrLn "Day 15: Best cookies"
  ings <- (fmap parse . lines) `fmap` readFile "input"

  print ings

  print $ maximum $ (totalScore . score) `fmap` allRecipes ings maxIngredients
  print $ maximum $ totalScore `fmap` (filter ((== 500) . totalCalories) (score `fmap` allRecipes ings maxIngredients))

