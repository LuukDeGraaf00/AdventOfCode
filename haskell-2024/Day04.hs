module Day04 where

import Data.List
import Data.Functor
import Data.Maybe

-- | Extract output.
output1 :: IO Int
output1 = readFile "Day4Input.txt" <&> (part1 . lines)

-- | Extract output.
output2 :: IO Int
output2 = readFile "Day4Input.txt" <&> (part2 . lines)

-- | Part 1.
part1 :: [String] -> Int
part1 texts = sum $ map xmas (texts ++ transpose texts ++ diagonal texts ++ diagonal (map reverse texts))

-- | Part 2.
part2 :: [String] -> Int
part2 texts = foldr (\ix -> (mas (finder texts ix) +)) 0  [[(r, c), (r, c + 2), (r + 1, c + 1), (r + 2, c), (r + 2, c + 2)] | r <- [0..length (head texts)], c <- [0..length texts]] 

-- | Diagonal line
diagonal :: [String] -> [String]
diagonal texts   = map dia line
    where width  = length (head texts)
          height = length texts
          line   = map (,0) [0..width] ++ map (0,) [1..height]
          dia (column, row) = if column < width && row < height
            then ((texts !! row) !! column) : dia (column + 1, row + 1)
            else []

-- | Find all values of these indicies.
finder :: [[a]] -> [(Int, Int)] -> [a]
finder xs = mapMaybe (\(r, c) -> (xs !? c) >>= (!? r))

-- | Lookup
(!?) :: [a] -> Int -> Maybe a
xs !? n | n < 0     = Nothing
        | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n

-- | Matches on XMAS string.
xmas :: String -> Int
xmas ('X':'M':'A':'S': xs) = 1 + xmas ('M':'A':'S': xs)
xmas ('S':'A':'M':'X': xs) = 1 + xmas ('A':'M':'X': xs)
xmas (x : xs)              = xmas xs
xmas _                     = 0

-- | Matches on X-MAS string.
mas :: String -> Int
mas "MSAMS" = 1
mas "MMASS" = 1
mas "SSAMM" = 1
mas "SMASM" = 1
mas _       = 0