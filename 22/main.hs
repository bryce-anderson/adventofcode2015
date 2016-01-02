import Debug.Trace


import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Control.Applicative

data SpellT = MMissile
            | Drain
            | Shield
            | Poison
            | Recharge
        deriving (Show, Eq, Ord)

allSpells = S.fromList [ MMissile
                       , Drain
                       , Shield
                       , Poison
                       , Recharge ]

              --      name   cost dur heal rec dm sh
spellMM       = Spell MMissile 53  1  0    0   4   0
spellDrain    = Spell Drain    73  1  2    2   0   0
spellShield   = Spell Shield   113 6  0    0   0   7
spellPoison   = Spell Poison   173 6  0    0   3   0
spellRecharge = Spell Recharge 229 5  0    101 0   0

newSpells :: M.Map SpellT Spell
newSpells = M.fromList ((\s -> (spell s, s)) <$> all) where
  all = [ spellMM
        , spellDrain
        , spellShield
        , spellPoison
        , spellRecharge ]

data Spell = Spell { spell    :: SpellT
                   , cost     :: Int
                   , duration :: Int
                   , heal     :: Int
                   , recharge :: Int
                   , damage   :: Int
                   , shield   :: Int }
        deriving (Show)

tickSpell :: Spell -> [Spell]
tickSpell s | duration s == 1 = []
            | otherwise = [s { duration = (duration s - 1) }]

data Boss = Boss { bhealth :: Int
                 , dmg     :: Int }
        deriving (Show)

data Player = Player { phealth :: Int
                     , mana    :: Int
                     , spells  :: [Spell] }
       deriving (Show)

data GameState = GameState { player :: Player
                           , boss :: Boss
                           , spent :: Int }
      deriving (Show)

applyTurn :: GameState -> GameState
applyTurn = playerTurn where
  bossTurn (GameState p@(Player ph m ss) b@(Boss _ bd) sp)
    = GameState p' b sp
      where
        p' = p { phealth = max 0 (ph-att)
               , spells = tickSpells ss
               , mana = m + rec }
        att = max 1 (bd-sh)
        sh = sum $ shield <$> ss
        rec = sum $ recharge <$> ss


  playerTurn (GameState p@(Player ph m ss) (Boss bh bd) sp)
    | bh <= dm  = GameState p' (Boss 0 bd) sp
    | otherwise = bossTurn $ GameState p' (Boss (bh-dm) bd) sp

     where
       p' = p { spells = tickSpells ss, mana = m + rec, phealth = ph + h }
       rec = sum $ recharge <$> ss
       h   = sum $ heal <$> ss
       dm  = sum $ damage <$> ss

  tickSpells :: [Spell] -> [Spell]
  tickSpells [] = []
  tickSpells (s:ss) | duration s <= 1 = tickSpells ss
                    | otherwise = s { duration = (duration s - 1) } : tickSpells ss


-- runs two turns: one for the player and once for the boss
minMana :: GameState -> Maybe Int
minMana g@(GameState p@(Player _ m ss) boss sp) = mincost where

  mincost = case costs of
              [] -> Nothing
              cs -> Just $ minimum cs

  costs = trace ("Spent: " ++ show sp) $ concat $ recurse <$> nextStates

  recurse (GameState _ (Boss 0 _) spent) = [spent] -- defeated the boss
  recurse g = case minMana g of
                   Just i -> [i]
                   Nothing -> []

  -- valid next states
  nextStates = filter ((>0). phealth . player) $ applyTurn <$> (addSpell <$> availableSpells)
  addSpell s = g { player = p { mana = (mana p)-(cost s), spells = s:spells p }, spent = sp+ (cost s) }
  availableSpells = filter ((<=m) . cost) $
                    fmap (newSpells M.!) $
                    S.toList $
                    S.difference allSpells (S.fromList (spell <$> ss))

initial = GameState (Player 50 500 []) (Boss 58 9) 0

main = do
  putStrLn "Day 22: Wizards and Monsters"
  print $ minMana initial

