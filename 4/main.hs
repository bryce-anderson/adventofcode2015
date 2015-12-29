{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (isPrefixOf)

key :: BS.ByteString
key = "ckczppom"

firstHit cnt = go 0 where
  prefix = take cnt $ repeat '0'
  go i = let h = show $ md5 (key <> (BS.pack $ show i))
         in if isPrefixOf prefix h then i
            else go (i+1)

main =do
  putStrLn "Day 4: Crypto currency"
  putStrLn $ "5 zeros: " ++ show (firstHit 5)
  putStrLn $ "6 zeros: " ++ show (firstHit 6)
