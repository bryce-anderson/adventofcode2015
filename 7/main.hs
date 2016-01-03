{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State.Strict
import Control.Applicative

import Data.Array

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Char
import Data.Word
import Data.Bits

type WireID = T.Text

data Op = AND WireID WireID
        | OR WireID WireID
        | NOT WireID
        | SHIFT WireID Int
        | LIT Word16
        | ID WireID
    deriving (Show)

data WireSpec = WireSpec { wire :: WireID
                         , op :: Op }
    deriving (Show)

parse :: T.Text -> [WireSpec]
parse txt = execState all [] where
  ls = T.lines txt

  all = forM_ ls $ \l -> modify ((parseLine l):)

  parseLine l = result where
    words = T.words l
    result = case words of
      [a,"AND",b,"->",t] -> WireSpec t $ AND a b
      [a,"OR", b,"->",t] -> WireSpec t $ OR a b
      ["NOT",a,"->",t]   -> WireSpec t $ NOT a
      [a,"RSHIFT",b,"->",t] -> WireSpec t $ SHIFT a (-1*(read $ T.unpack b))
      [a,"LSHIFT",b,"->",t] -> WireSpec t $ SHIFT a (read $ T.unpack b)
      [a,"->",t]
        | isNumber (head a') -> WireSpec t $ LIT (read a')
        | otherwise -> WireSpec t $ ID a
          where a' = T.unpack a
      _ -> error $ "Invalid line: " ++ T.unpack l


probe :: [WireSpec] -> [WireID] -> [Word16]
probe spec requested = fetchId <$> requested where
  ids = wire <$> spec
  idMap = M.fromList $ (ids `zip` [0..])

  program = M.fromList (ps <$> spec) where
    ps (WireSpec id op) = (id,op)

  arrId :: WireID -> Word16
  arrId w
    | Just intId <- M.lookup w idMap = idArray ! intId
    | otherwise = error $ "Invalid id: " ++ show w

  idArray :: Array Int Word16
  idArray = listArray (0, M.size idMap) $ fetchId <$> ids

  fetchId :: WireID -> Word16
  fetchId id
    | Just op <- M.lookup id program = go op
    | otherwise = error $ "Unknown wire: " ++ show id
    where
      intId = idMap M.! id
      go o = case o of
        AND a b    -> (arrId a) .&. (arrId b)
        OR a b     -> (arrId a) .|. (arrId b)
        NOT a      -> complement (arrId a)
        SHIFT a s -> shift (arrId a) s
        ID a       -> arrId a
        LIT r      -> r


main = do
  putStrLn "Day 7: Some Assembly Required"
  input <- parse <$> T.readFile "input"
  -- add the value of "1" because it comes up in AND gates
  let inputWithOne = (input ++ [WireSpec "1" (LIT 1)])

  T.putStrLn "Part 1:"
  let [res1] = probe inputWithOne ["a"]
  print res1

  T.putStrLn "Part 2:"
  -- Adding the value to the end acts as an 'override'
  print $ probe (inputWithOne ++ [WireSpec "b" (LIT res1)]) ["a"]


