{-# LANGUAGE OverloadedStrings #-}

module Day11 where

import Data.Functor ((<&>))

import Data.IntMap qualified as M

output1 :: IO Int
output1 = readFile "Day11Input.txt" <&> solve 25 . parse

output2 :: IO Int
output2 = readFile "Day11Input.txt" <&> solve 75 . parse

solve :: Foldable t => Int -> t Int -> Int
solve n = fst . foldr f (0, replicate n mempty)
    where f a (b1, m) = let (x,y) = blink (a, m) in (b1 + x, y)

blink :: (Int, [M.IntMap Int]) -> (Int, [M.IntMap Int])
blink (_, [])     = (1, [])
blink (0, m : ms) = let (r, m0) = blink (1, ms) in (r, m : m0)
blink (x, m : ms) = maybe (if even size then r1 else r0) (, m : ms) (M.lookup x m)
        where size   = floor $ logBase 10 (fromIntegral x) + 1
              (l, r) = x `divMod` (10 ^ (size `div` 2))
              r0     = let (a0, m0) = blink (x * 2024, ms) in (a0, M.insert x a0 m : m0)
              r1     = let (a1, m1) = blink (l, ms) in let (a2, m2) = blink (r, m1) in (a1 + a2, M.insert x (a1 + a2) m : m2)

parse :: String -> [Int]
parse = Prelude.map read . Prelude.words