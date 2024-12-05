module Day5 where

import Data.List
import Data.Functor
import Data.Char
import Data.Maybe
import qualified Data.Map as Map

-- | Extract output.
output1 :: IO Int
output1 = readFile "Day5Input.txt" <&> part1 . parse1 mempty 

-- | Extract output.
output2 :: IO Int
output2 = readFile "Day5Input.txt" <&> part2 . parse1 mempty 

-- | Compute output.
part1 :: (Map.Map Int [Int], [[Int]]) -> Int
part1 (m, xs) = foldr (\a b -> fromMaybe 0 (valid m [] a) + b) 0 xs 

-- | Compute output.
part2 :: (Map.Map Int [Int], [[Int]]) -> Int 
part2 (m, xs) = foldr (\a b -> if isNothing (valid m [] a) then fix m a + b else b) 0 xs 

-- | Check if updates are valid.
valid :: Map.Map Int [Int] -> [Int] -> [Int] -> Maybe Int
valid m before []          = Just (median before)
valid m before (n : after) = let next = valid m (before ++ [n]) after in case Map.lookup n m of
    Just vs -> if null (before `intersect` vs) then next else Nothing
    Nothing -> next

-- | Fix order.
fix :: Map.Map Int [Int] -> [Int] -> Int
fix m = median . sortBy (\a b -> if isJust (Map.lookup a m >>= find (== b)) then LT else GT)

-- | Compute median.
median :: [a] -> a
median xs = xs !! (length xs `div` 2)

-- | Parse rules.
parse1 :: Map.Map Int [Int] -> String -> (Map.Map Int [Int], [[Int]])
parse1 m []          = (m, mempty)
parse1 m ('\n' : xs) = (m, parse2 xs)
parse1 m xs          = let (ls, '|' : ys) = span isDigit xs
                           (rs, '\n': zs) = span isDigit ys 
                       in parse1 (Map.insertWith (++) (read ls :: Int) [read rs :: Int] m) zs

-- | Parse updates.
parse2 :: String -> [[Int]]
parse2 xs = map (map read . comma) (lines xs)
    where comma s = case dropWhile (== ',') s of
            "" -> []
            s' -> let (w, s'') = break (== ',') s' in w : comma s''