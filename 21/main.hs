import Data.List (partition)

data Item = Item { name :: String
                 , cost :: Int
                 , damage :: Int
                 , armor :: Int }
    deriving (Show, Eq, Ord)

emptyItem = Item "None" 0 0 0

weapons = [ Item "Dagger" 8 4 0
          , Item "Shortsword" 10 5 0
          , Item "Warhammer" 25 6 0
          , Item "Longsword" 40 7 0
          , Item "Greataxe" 74 8 0 ]

armors = emptyItem :
         [ Item "Leather" 13 0 1
         , Item "Chainmail" 31 0 2
         , Item "Splintmail" 53 0 3
         , Item "Bandedmail" 75 0 4
         , Item "Platemail" 102 0 5 ]

rings = [ Item "Damage+1" 25 1 0
        , Item "Damage+2" 50 2 0
        , Item "Damage+3" 100 3 0
        , Item "Defense+1" 20 0 1
        , Item "Defense+2" 40 0 2
        , Item "Defense+3" 80 0 3 ]

sumItems (Item n c d a) (Item n' c' d' a') =
  Item (n++n') (c+c') (d+d') (a+a')

ringCombos = emptyItem : (rings ++ go rings) where
  go (i:is) = (sumItems i `fmap` is) ++ go is
  go [] = []

-- generates all the combos of configurations taking one from each category
configs = go [weapons, armors, ringCombos] where
  go [is] = is
  go (c:cs) = concat (f `fmap` c) where
   f i = sumItems i `fmap` (go cs)

data Player = Player { pts :: Int
                     , dmg :: Int
                     , arm :: Int }
  deriving (Eq, Ord, Show)

fight boss player = odd (go 1 player boss) where
  go i p1@(Player _ d _) p2@(Player p' _ a')
    | p' <= att = i
    | otherwise = go (i+1) p2' p1
    where
      att = max 1 (d-a')
      p2' = p2 { pts = (p'-att) }

fightConfig boss (Item _ c d a) = fight boss p where
  p = Player 100 d a

boss = Player 104 8 1

main = do
  putStrLn "Day 21: Video Game Hero"
  let (winning,losing) = partition (fightConfig boss) configs
      winningcosts = cost `fmap` winning
      losingcosts = cost `fmap` losing

  putStr "Minimum to win: "
  print $ minimum winningcosts

  putStr "Maximum to lose: "
  print $ maximum losingcosts

