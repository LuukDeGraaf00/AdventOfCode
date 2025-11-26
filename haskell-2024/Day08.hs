module Day08 where

import Data.Functor
import Data.Map qualified as Map
import Data.List (tails, nub, sort)

type Pos    = (Int, Int)
type Result = ((Int, Int), Map.Map Char [Pos]) 

output1 :: IO Int
output1 = readFile "Day8Input.txt" <&> length . compute part1 . parse ((0, 0), mempty)

output2 :: IO Int
output2 = readFile "Day8Input.txt" <&> length . compute part2 . parse ((0, 0), mempty)

-- | Compute function that enumerates solution.
compute :: ((Int, Int) -> [Pos] -> [Pos]) -> Result -> [Pos]
compute f (size, m) = Map.foldr' (\a b -> filter (`notElem` b) (f size a) ++ b) [] m

-- | Compute all antennas.
part1 :: (Int, Int) -> [Pos] -> [Pos]
part1 size xs = [n | (x : ys) <- tails xs, y <- ys, n <- antenna x y, inbounds size n]
    where antenna (x, y) (a, b) = let (l, r) = (x - a, y - b) in [(x + l, y + r), (a - l, b - r)]

-- | Compute all antennas in line.
part2 :: (Int, Int) -> [Pos] -> [Pos]
part2 size xs = nub [n | (x : ys) <- tails xs, y <- ys, n <- antenna x y]
    where antenna (x, y) (a, b) = let (l, r) = (x - a, y - b) 
                                      z      = takeWhile (inbounds size) $ iterate (\(x, y) -> (x + l, y + r)) (x, y)
                                      q      = takeWhile (inbounds size) $ iterate (\(a, b) -> (a - l, b - r)) (a, b)
                                  in z ++ q

-- | Value is inbounds.
inbounds :: (Int, Int) -> Pos -> Bool
inbounds (x, y) (a, b) = 0 <= a && a < x && 0 <= b && b < y

-- | Parses
parse :: Result -> String -> Result
parse ((x, y), m) []          = ((x, y + 1), m)
parse ((x, y), m) ('\n' : cs) = parse ((0, y + 1), m) cs
parse ((x, y), m) ('.'  : cs) = parse ((x + 1, y), m) cs
parse ((x, y), m) ('#'  : cs) = parse ((x + 1, y), m) cs
parse ((x, y), m) (c    : cs) = parse ((x + 1, y), Map.insertWith (++) c [(x, y)] m) cs
        
