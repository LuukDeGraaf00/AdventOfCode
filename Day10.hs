module Day10 where

-- for each 0 -> figure out how many 9 you can find 

import Prelude as P
import Data.Vector.Unboxed as V
import Data.Char
import Data.Functor ((<&>))
import Debug.Trace
import Data.List (nub)

data Grid = Grid 
    {
        grid   :: Vector Int,
        width  :: Int,
        height :: Int
    } deriving Show

output1 :: IO Int
output1 = readFile "Day10Input.txt" <&> part1 . parse

output2 :: IO Int
output2 = readFile "Day10Input.txt" <&> part2 . parse

part1 :: Grid -> Int
part1 gs@(Grid g w h)    = ifoldr' function 0 g
    where function i 0 b = P.length (nub $ solve 0 (i `mod` w, i `div` w) gs) + b
          function _ _ b = b

part2 :: Grid -> Int
part2 gs@(Grid g w h)    = ifoldr' function 0 g
    where function i 0 b = solve2 0 (i `mod` w, i `div` w) gs + b
          function _ _ b = b

solve :: Int -> (Int, Int) -> Grid -> [(Int, Int)]
solve 9     (x, y) g = [(x, y) | get (x, y) g == Just 9]
solve level (x, y) g = if get (x, y) g == Just level then solve (level + 1) (x + 1, y) g P.++
                                                          solve (level + 1) (x - 1, y) g P.++
                                                          solve (level + 1) (x, y + 1) g P.++
                                                          solve (level + 1) (x, y - 1) g
                                                     else []

solve2 :: Int -> (Int, Int) -> Grid -> Int
solve2 9     (x, y) g = fromEnum (get (x, y) g == Just 9)
solve2 level (x, y) g = if get (x, y) g == Just level then solve2 (level + 1) (x + 1, y) g +
                                                           solve2 (level + 1) (x - 1, y) g +
                                                           solve2 (level + 1) (x, y + 1) g +
                                                           solve2 (level + 1) (x, y - 1) g
                                                      else 0

get :: (Int, Int) -> Grid -> Maybe Int
get (x, y) (Grid g w h) | x < w && y < h && x >= 0 && y >= 0 = Just (g ! (x + (y * w)))
                        | otherwise                          = Nothing

parse :: String -> Grid
parse xs = let (g, w, h) = P.foldr f ([], 0, 1) xs in Grid (fromList g) w h
    where f '.'  (xs, w, h) = (10 : xs, w + 1, h)
          f '\n' (xs, w, h) = (xs, 0, h + 1)
          f  x   (xs, w, h) = ((digitToInt x :: Int) : xs, w + 1, h)