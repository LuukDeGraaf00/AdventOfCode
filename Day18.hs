module Day18 where
import Data.Functor
import qualified Data.Set as S
import Debug.Trace (trace)

output1 :: IO Int
output1 = readFile "Day18Input.txt" <&> part1 (S.singleton (0, 0)) (70, 70) . S.fromList . take 1024 . parse

output2 :: IO (Int, Int)
output2 = readFile "Day18Input.txt" <&> part2 0 1725 3450 . parse

parse :: String -> [(Int, Int)]
parse xs = let (l, ',' : ls) = break (== ',') xs in case break (== '\n') ls of
    (r, []) -> [(read l, read r)]
    (r, rs) -> (read l, read r) : parse rs

part1 :: S.Set (Int, Int) -> (Int, Int) -> S.Set (Int, Int) -> Int
part1 queue (l, r) invalid | S.null queue        = error "No path!"
part1 queue (l, r) invalid | (l, r) `elem` queue = 0
                           | otherwise           = 1 + part1 options (l, r) (options <> invalid)
    where options          = S.filter valid $ S.foldr (\a b -> foldr S.insert b (neighbors a)) S.empty queue
          valid (a, b)     = a >= 0 && b >= 0 && a <= l && b <= r && (a, b) `notElem` invalid

exists :: S.Set (Int, Int) -> (Int, Int) -> S.Set (Int, Int) -> Bool
exists queue (l, r) invalid | S.null queue        = False
                            | (l, r) `elem` queue  = True
                            | otherwise            = exists options (l, r) (options <> invalid)
    where options          = S.filter valid $ S.foldr (\a b -> foldr S.insert b (neighbors a)) S.empty queue
          valid (a, b)     = a >= 0 && b >= 0 && a <= l && b <= r && (a, b) `notElem` invalid

part2 :: Int -> Int -> Int -> [(Int, Int)] -> (Int, Int)
part2 l n r xs | n == l || n == r = xs !! n 
               | otherwise        = trace (show n) $ if exists (S.singleton (0,0)) (70, 70) (S.fromList (take n xs)) 
                    then part2 n (n + ((r - n) `div` 2)) r xs
                    else part2 l (l + ((n - l) `div` 2)) n xs

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (y, x) = [(y, x - 1), (y - 1, x), (y, x + 1), (y + 1, x)]
