module Day7 where

import Data.List
import Data.Functor

output1 :: IO Int
output1 = readFile "Day7Input.txt" <&> part1 . parse

output2 :: IO Int
output2 = readFile "Day7Input.txt" <&> part2 . parse2

part1 :: [(Int, [Int])] -> Int
part1 = foldr (\a r -> if valid a then fst a + r else r) 0

part2 :: [(Int, [String])] -> Int
part2 = foldr (\a r -> if valid2 a then fst a + r else r) 0

valid :: (Int, [Int]) -> Bool
valid (total, [result])       = total == result
valid (total, x : y : values) = x <= total && 
    (valid (total, (x + y) : values) || 
     valid (total, (x * y) : values))

valid2 :: (Int, [String]) -> Bool
valid2 (total, [result])       = total == read result
valid2 (total, x : y : values) = let (n, m) = (read x :: Int, read y :: Int) in n <= total && 
    (valid2 (total, show (n + m) : values) || 
     valid2 (total, show (n * m) : values) || 
     valid2 (total, (x ++ y) : values))
    
parse :: String -> [(Int, [Int])]
parse = map (\x -> let (t, ':' : rs) = break (== ':') x in (read t, map read $ words rs)) . lines

parse2 :: String -> [(Int, [String])]
parse2 = map (\x -> let (t, ':' : rs) = break (== ':') x in (read t, words rs)) . lines
