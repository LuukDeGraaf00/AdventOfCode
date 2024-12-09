module Day9 where

import Data.Sequence as S
import Data.Char (digitToInt, intToDigit)
import Data.Functor

output1 :: IO Int
output1 = readFile "Day9Input.txt" <&> part1 0 . parse 0 True

output2 :: IO Int
output2 = readFile "Day9Input.txt" <&> checksum2 0 . part2 . parse 0 True

checksum :: Int -> Int -> Int -> Int
checksum id size index = id * size * index + id * size * (size - 1) `div` 2

part1 :: Int -> Seq (Int, Int) -> Int
part1 i S.Empty                               = 0
part1 i (xs :|> (-1, _))                      = part1 i xs
part1 i ((-1, space) :<| (xs :|> (id, size))) = case compare space size of 
    EQ -> checksum id size  i + part1 (i + size ) xs
    GT -> checksum id size  i + part1 (i + size ) ((-1, space - size) :<| xs)
    LT -> checksum id space i + part1 (i + space) (xs :|> (id, size - space))
part1 i ((id, size) :<| xs)                   = checksum id size i + part1 (i + size) xs

part2 :: Seq (Int, Int) -> Seq (Int, Int)
part2 S.Empty              = S.Empty
part2 (xs :|> y@(_, size)) = case next y xs of 
    Just v  -> part2 v  :|> (-1, size)
    Nothing -> part2 xs :|> y

checksum2 :: Int -> S.Seq (Int, Int) -> Int
checksum2 i ((-1, size) :<| xs) = checksum2 (i + size) xs
checksum2 i ((id, size) :<| xs) = checksum id size i + checksum2 (i + size) xs
checksum2 _ S.Empty             = 0

-- | Computes next position.
next :: (Int, Int) -> Seq (Int, Int) -> Maybe (Seq (Int, Int))
next (-1, _) xs                       = Just xs
next v S.Empty                        = Nothing
next (n, size) (x@(-1, space) :<| xs) = case compare space size of
    EQ -> Just ((n, size) :<| xs)
    GT -> Just ((n, size) :<| (-1, space - size) :<| xs)
    LT -> next (n, size) xs >>= (\v -> Just (x :<| v))
next v (x :<| xs)                     = next v xs >>= (\v -> Just (x :<| v))

parse :: Int -> Bool -> String -> Seq (Int, Int)
parse n b []       = S.empty
parse n b (c : cs) = id :<| parse nx (not b) cs
    where id = (if b then n else -1, digitToInt c)
          nx = if b then n else n + 1
    