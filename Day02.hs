module Day02 where
    
import Data.Functor
import Data.List

-- | Extract output.
output1 :: IO Int
output1 = readFile "Day2Input.txt" <&> (count safe . parse)

-- | Extract output.
output2 :: IO Int
output2 = readFile "Day2Input.txt" <&> (count safe2 . parse)

-- | Count occurrences of safe report.
count :: ([Int] -> Bool) -> [[Int]] -> Int
count f = foldr (\a b -> (if f a then 1 else 0) + b) 0 

-- | Determines safety.
safe :: (Ord a, Num a) => [a] -> Bool
safe xs@(x : y : _) = and (zipWith (direction x y) xs (tail xs))

-- | Enumerate all solutions.
safe2 :: [Int] -> Bool
safe2 list = any safe (list : zipWith (++) (inits list) (tail $ tails list))

-- | Determines direction.
direction :: (Ord a, Num a) => a -> a -> (a -> a -> Bool)
direction a b = if a < b then f else flip f
    where f x y = x < y && y <= x + 3

-- | Parsing function.
parse :: String -> [[Int]]
parse = map (map (read @Int) . words) . lines