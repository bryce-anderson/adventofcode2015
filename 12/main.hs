{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.Aeson
import Data.Foldable (foldMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BS


numbers p v = numbers' v where
  numbers' (Number n) = [round n]
  numbers' (Array vs) = concat $ numbers' `fmap` V.toList vs
  numbers' (Object vs) | p vs = foldMap numbers' vs
                      | otherwise = []
  numbers' _ = []


main = do
  putStrLn "Day 12: Json Accounting"
  Just json <- decode `fmap` BS.readFile "input" :: IO (Maybe Value)

  print $ sum $ numbers (const True) json
  print $ sum $ numbers (not . any (== "red") . HM.elems) json




