import Data.Char
import Data.Map.Strict as M
import Data.Foldable (for_)

data Token = Token { name :: String
                   , value :: Int }
   deriving (Show, Eq, Ord)

type Aunt = (Token,Token,Token)

parseAunt l = (Token (st a) (rd na), Token (st b) (rd nb),Token (st c) (rd nc)) where
  st = takeWhile (/= ':')
  rd = read . takeWhile isDigit
  [_,_,a,na,b,nb,c,nc] = words l


detected :: [Token]
detected = [ Token "children" 3
           , Token "cats" 7
           , Token "samoyeds" 2
           , Token "pomeranians" 3
           , Token "akitas" 0
           , Token "vizslas" 0
           , Token "goldfish" 5
           , Token "trees" 3
           , Token "cars" 2
           , Token "perfumes" 1 ]


detectedMap = M.fromList $ (\(Token a b) -> (a,b)) `fmap` detected

auntMatches (Token a ac, Token b bc, Token c cc) =
  match a ac && match b bc && match c cc where
    match a b = case M.lookup a detectedMap of
                Just b' -> b' == b
                Nothing -> True

detectedMap2 = M.fromList  [ ("children", (==3))
                           , ("cats", (> 7))
                           , ("samoyeds", (==2))
                           , ("pomeranians", (< 3))
                           , ("akitas", (==0))
                           , ("vizslas", (==0))
                           , ("goldfish", (< 5))
                           , ("trees", (> 3))
                           , ("cars", (==2))
                           , ("perfumes", (==1)) ]

auntMatches2 (Token a ac, Token b bc, Token c cc) =
  match a ac && match b bc && match c cc where
    match a b = case M.lookup a detectedMap2 of
                Just p -> p b
                Nothing -> True

getMatch results = take 1 rs where
  rs = dropWhile (not . snd) $ [1..] `zip` results

main = do
  putStrLn "Day 16: Aunt Sue"
  aunts <- (fmap parseAunt . lines) `fmap` readFile "input"

  print $ getMatch $ auntMatches `fmap` aunts
  print $ getMatch $ auntMatches2 `fmap` aunts

