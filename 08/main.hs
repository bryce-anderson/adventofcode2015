import Data.Char

import Data.Foldable
import Prelude hiding (sum)

decodeLine :: String -> String
decodeLine = go . drop 1 where
  go ['"'] = []
  go ('\\':'\\':xs) = '\\' : go xs
  go ('\\':'"':xs) = '"' : go xs
  go ('\\':'x':a:b:xs) = chr ((digitToInt a)*16 + digitToInt b) : go xs
  go (a:as) = a : go as
  go [] = error "Unterminated line"

encodeLine :: String -> String
encodeLine s = '"' : go s where
  go ('"':xs) = '\\':'"':go xs
  go ('\\':xs) = '\\':'\\':go xs
  go (x:xs) = x:go xs
  go [] = ['"']

chardiffs sz = go sz 0 where
  go (s:sz) acc = go sz (acc + length s - length (decodeLine s))
  go [] acc = acc

main = do
  putStrLn "Day 8: Encoded strings."
  strings <- lines `fmap` readFile "input"

  let sumLens = sum $ length `fmap` strings

  forM_ strings $ \s ->
    putStrLn $ s ++ ": " ++ decodeLine s

  print $ sumLens - (sum $ (length . decodeLine) `fmap` strings)
  print $ (sum $ (length . encodeLine) `fmap` strings) - sumLens


