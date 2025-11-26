module Day22 where

import Data.Bits (xor)
import Data.Functor

import Data.Map qualified as M

output1 :: IO Int
output1 = readFile "Day22Input.txt" <&> sum . map (secrets 2000 . read) . lines

output2 :: IO Int
output2 = readFile "Day22Input.txt" <&> M.foldr max 0 . M.map sum . foldr1 (M.unionWith (++)) . map (prices . read) . lines 

secret :: Int -> Int
secret = step (* 2048) . step (`div` 32) . step (* 64)
    where step f x = (x `xor` f x) `mod` 16777216

secrets :: Int -> Int -> Int
secrets n x = foldr id x (replicate n secret)

prices :: Int -> M.Map [Int] [Int]
prices s = M.fromList (take 2000 (record (drop 3 values) changes))
  where record vs cs = (take 4 cs, [head vs]) : record (tail vs) (tail cs)
        changes      = 0 : zipWith (-) (tail values) values
        values       = map (`mod` 10) (iterate secret s)


        
