module Day1 where

import Data.List (sort, uncons)
import Data.Maybe (fromMaybe)
import Data.Functor ((<&>))
import System.IO

-- | Extract output.
output1 :: IO Int
output1 = readFile "Day1Input.txt" <&> (part1 . parse)

-- | Extract output.
output2 :: IO Int
output2 = readFile "Day1Input.txt" <&> (part2 . parse)

-- | Part 1 function.
part1 :: (Num a, Ord a) => ([a], [a]) -> a
part1 (xs, ys) = sum (zipWith difference (sort xs) (sort ys))

-- | Part 2 function.
part2 :: ([Int], [Int]) -> Int 
part2 (xs, ys) = foldr (\a -> (similarity a ys +)) 0 xs

-- | Difference between two numbers.
difference :: Num a => a -> a -> a
difference x y = abs (x - y)

-- | Compute similarity of value. 
similarity :: (Foldable t, Num b, Eq b) => b -> t b -> b
similarity value = foldr (\x -> ((if value == x then value else 0) +)) 0

-- | Parsing function.
parse :: String -> ([Int], [Int])
parse [] = ([], [])
parse xs = (read left : ls, read right : rs)
    where (left,  _ : ys) = break (== ' ')  xs
          (right,     zs) = break (== '\n') ys
          (ls, rs)        = parse (maybe mempty snd (uncons zs))