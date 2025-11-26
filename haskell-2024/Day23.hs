module Day23 where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Functor ((<&>))
import Data.List (maximumBy, sort)
import Data.Ord (comparing)

type Graph = M.Map (Char, Char) [(Char, Char)]

output1 :: IO Int
output1 = readFile "Day23Input.txt" <&> length . part1 . parse

output2 :: IO [(Char, Char)]
output2 = readFile "Day23Input.txt" <&> sort . S.toList . maximumBy (comparing S.size) . part2 . parse

parse :: String -> Graph
parse xs = foldr f mempty (lines xs)
    where f [a, b, _, c, d] = M.insertWith (\[v] -> (v :)) (c, d) [(a, b)] .
                              M.insertWith (\[v] -> (v :)) (a, b) [(c, d)]

part1 :: Graph -> S.Set ((Char, Char), (Char, Char), (Char, Char))
part1 m = M.foldrWithKey (\v1@(n, _) vs b -> if n == 't' then s1 v1 vs b else b) mempty m
    where s1 v1 vs b          = foldr S.insert b (concatMap (s2 v1) vs)
            where s2 v1 v2    = maybe [] (concatMap (s3 v1 v2)) (M.lookup v2 m)
                  s3 v1 v2 v3 | v1 /= v3 && elem v3 vs = [tuple v1 v2 v3]
                              | otherwise              = []

part2 :: Graph -> [S.Set (Char, Char)]
part2 m = go S.empty (M.keysSet m) S.empty
  where go clique yes no = case S.minView yes of
            Nothing -> [clique | S.size no == 0]
            Just (v, r) -> let vs = S.fromList $ m M.! v 
                in go (S.insert v clique) (S.intersection yes vs) (S.intersection no vs)
                <> go clique r (S.insert v no)

tuple :: Ord a => a -> a -> a -> (a, a, a)
tuple x y z
  | x <= y && x <= z = if y <= z then (x, y, z) else (x, z, y)
  | y <= x && y <= z = if x <= z then (y, x, z) else (y, z, x)
  | otherwise        = if x <= y then (z, x, y) else (z, y, x)
