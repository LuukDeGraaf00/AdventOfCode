module Day21 where

import Data.Map qualified as M
import Data.List 
import Data.Functor

output1 :: IO Int
output1 = readFile "Day21Input.txt" <&> sum . map (compute 2) . words

output2 :: IO Int
output2 = readFile "Day21Input.txt" <&> sum . map (compute 25) . words

compute :: Int -> String -> Int
compute depth input = read (init input) * solve options depth input

options :: M.Map (Char, Char) [String]
options = M.fromList [((a, b), path a b) | a <- "A0123456789a<v>^", 
                                           b <- "A0123456789a<v>^"]

solve :: M.Map (Char, Char) [String] -> Int -> String -> Int
solve table depth = fst . go M.empty (depth + 1) 'A'
  where
    go memo 0 _     s = (length s, memo)
    go memo n start s = foldl' (search n) (0, memo) (zip (start : s) s)

    search n (r, m0) p = case (n, p) `M.lookup` m0 of
        Just v0 -> (r + v0, m0)
        Nothing -> (r + v1, M.insert (n, p) v1 m3)
            where (v1, m3) = case table M.! p of
                    [p]      -> go m0 (n - 1) 'a' p
                    [p1, p2] -> if x1 < x2 then (x1, m2) else (x2, m2)
                        where (x1, m1) = go m0 (n - 1) 'a' p1
                              (x2, m2) = go m1 (n - 1) 'a' p2

path :: Char -> Char -> [String]
path a b | a == '<' || x1 == 0 && y2 == 3 = [w ++ h ++ "a"]
         | b == '<' || x2 == 0 && y1 == 3 = [h ++ w ++ "a"]
         | null w   || null h             = [h ++ w ++ "a"]
         | otherwise                      = [h ++ w ++ "a", w ++ h ++ "a"]
  where (x1, y1) = pos a 
        (x2, y2) = pos b
        w = replicate (abs (x2 - x1)) (if x2 > x1 then '>' else '<')
        h = replicate (abs (y2 - y1)) (if y2 > y1 then 'v' else '^')

pos :: Char -> (Int, Int)
pos 'A' = (2, 3)
pos '0' = (1, 3)
pos '1' = (0, 2)
pos '2' = (1, 2)
pos '3' = (2, 2)
pos '4' = (0, 1)
pos '5' = (1, 1)
pos '6' = (2, 1)
pos '7' = (0, 0)
pos '8' = (1, 0)
pos '9' = (2, 0)
pos 'a' = (2, 0)
pos '^' = (1, 0)
pos '<' = (0, 1)
pos 'v' = (1, 1)
pos '>' = (2, 1)
