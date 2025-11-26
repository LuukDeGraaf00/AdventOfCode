module Day13 where

import Data.List
import Data.Functor ( (<&>) )
import Data.Maybe (fromMaybe)
import Data.Bifunctor 

output1 :: IO Int
output1 = readFile "Day13Input.txt" <&> parse 0

output2 :: IO Int
output2 = readFile "Day13Input.txt" <&> parse 10000000000000

parse :: Int -> String -> Int
parse offset xs = fromMaybe 0 (solve (aX, aY) (bX, bY) (tX + offset, tY + offset)) + if null r then 0 else parse offset (drop 2 r)
    where 
        number (c, n) (x, y)          = first ((: x) . read) (break (== c) (drop n y))  
        ([tY, tX, bY, bX, aY, aX], r) = foldr number ([], xs) [('\n', 4), (',', 10), ('\n', 4), (',', 13), ('\n', 4), (',', 12)]
        
solve :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Maybe Int
solve (aX, aY) (bX, bY) (tX, tY) = if r1 == 0 && r2 == 0 then Just (a * 3 + b) else Nothing
    where
        (b, r1) = (aY * tX - aX * tY) `divMod` (aY * bX - aX * bY)
        (a, r2) = (tX - bX * b) `divMod` aX