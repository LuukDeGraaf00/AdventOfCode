module Day16 where

import Data.Vector qualified as V
import Data.Functor ((<&>))
import Data.List (insertBy, nub)
import Data.Set qualified as S
import Debug.Trace

output1 = readFile "Day16Input.txt" <&> part1 . parse

output2 = readFile "Day16Input.txt" <&> part2 . parse

parse :: String -> (V.Vector Char, Int, (Int, Int), (Int, Int))
parse xs = (vs, bound, start, end)
    where vs    = V.fromList xs
          bound = V.length (V.takeWhile (/= '\n') vs) + 1
          start = maybe undefined (index bound) (V.elemIndex 'S' vs)
          end   = maybe undefined (index bound) (V.elemIndex 'E' vs)

part1 :: (V.Vector Char, Int, (Int, Int), (Int, Int)) -> Int
part1 (grid, bound, start, end) = recurse (options (0, ('>', start))) S.empty
    where recurse [] history                                                              = error ("No answer. Searched: " ++ show history)
          recurse (o@(cost, (c, (x,y))) : os) history | (x,y) == end                      = cost
                                                      | grid V.! ((y * bound) + x) == '#' = recurse os history
                                                      | otherwise                         = recurse inserted (S.insert (x,y) history)
                                                            where inserted = foldr (insertBy (\(a, _) (b, _) -> compare a b)) os trim
                                                                  trim     = filter (\(a, (c, (x, y))) -> (x, y) `notElem` history) (options o)

part2 :: (V.Vector Char, Int, (Int, Int), (Int, Int)) -> Int
part2 (grid, bound, start, end) = recurse (options2 (0, ('>', start), mempty)) S.empty
    where recurse []                              history                                     = error "No answer"
          recurse (o@(cost, (c, (x,y)), xs) : os) history | (x,y) == end                      = length (S.map snd (xs <> mconcat (map (\(k, (_, (a, b)), xs) -> if k == cost && (x,y) == (a,b) then xs else S.empty) os))) + 1
                                                          | grid V.! ((y * bound) + x) == '#' = recurse os history
                                                          | (c, (x,y)) `elem` xs              = recurse os history
                                                          | otherwise                         = traceShow cost (recurse inserted (S.insert (c, (x,y)) history))
                                                                where inserted = foldr (insertBy (\(a, _, _) (b, _, _) -> compare a b)) os trim
                                                                      trim     = filter (\(_, (c, (x, y)), _) -> (c, (x, y)) `notElem` history) (options2 o)
index :: Int -> Int -> (Int, Int)
index bound n = (n `mod` bound, n `div` bound)

options :: (Int, (Char, (Int, Int))) -> [(Int, (Char, (Int, Int)))]
options (cost, ('>', p@(x, y))) = [(cost + 1, ('>', (x + 1, y))), (cost + 1000, ('^', p)), (cost + 1000, ('v', p)), (cost + 2000, ('<', p))]
options (cost, ('<', p@(x, y))) = [(cost + 1, ('<', (x - 1, y))), (cost + 1000, ('v', p)), (cost + 1000, ('^', p)), (cost + 2000, ('>', p))]
options (cost, ('^', p@(x, y))) = [(cost + 1, ('^', (x, y - 1))), (cost + 1000, ('<', p)), (cost + 1000, ('>', p)), (cost + 2000, ('v', p))]
options (cost, ('v', p@(x, y))) = [(cost + 1, ('v', (x, y + 1))), (cost + 1000, ('>', p)), (cost + 1000, ('<', p)), (cost + 2000, ('^', p))]

options2 :: (Int, (Char, (Int, Int)), S.Set (Char, (Int, Int))) -> [(Int, (Char, (Int, Int)), S.Set (Char, (Int, Int)))]
options2 (cost, (c, (x, y)), xs) = map (\(a, b) -> (a, b, S.insert (c, (x,y)) xs)) (options (cost, (c, (x, y))))

