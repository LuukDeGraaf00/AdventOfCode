module Day17 where
import Data.Functor
import Data.Bits
import Control.Monad (guard)
import Data.List (isSuffixOf, tails, foldl')

output1 :: IO [Int]
output1 = readFile "Day17Input.txt" <&> part1 0 . parse

output2 :: IO Int
output2 = readFile "Day17Input.txt" <&> part2 . parse

parse :: String -> (Int, Int, Int, [Int])
parse x0 = (read a, read b, read c, i)
    where (a, x1)  = break (== '\n') (drop 12 x0)
          (b, x2)  = break (== '\n') (drop 13 x1)
          (c, x3)  = break (== '\n') (drop 13 x2)
          i        = map read (split (drop 11 x3))
          split    = foldr (\c l@(x:xs) -> if c == ',' then [] : l else (c : x) : xs) [[]] 

part1 :: Int -> (Int, Int, Int, [Int]) -> [Int]
part1 pt (a, b, c, xs) = let combo x = [0, 1, 2, 3, a, b, c] !! x in case drop pt xs of
        0 : x : _ -> part1 (pt+2) (a `shiftR` combo x, b, c, xs)
        1 : x : _ -> part1 (pt+2) (a, b `xor` x, c, xs)
        2 : x : _ -> part1 (pt+2) (a, combo x `mod` 8, b, xs)
        3 : x : _ -> part1 (if a == 0 then pt+2 else x) (a, b, c, xs)
        4 : _ : _ -> part1 (pt+2) (a, b `xor` c, c, xs)
        5 : x : _ -> combo x `mod` 8 : part1 (pt+2) (a, b, c, xs)
        6 : x : _ -> part1 (pt+2) (a, a `shiftR` combo x, c, xs)
        7 : x : _ -> part1 (pt+2) (a, b, a `shiftR` combo x, xs)
        _         -> []

part2 :: (a, Int, Int, [Int]) -> Int
part2 (_, y, z, xs) = maximum (foldl' go [0] (reverse xs))
    where go os t = [o * 8 + n | o <- os, n <- [0..7], head (part1 0 (o * 8 + n, y, z, xs)) == t]