module Day15 where

import Data.Vector qualified as V
import Data.Bifunctor
import Data.Functor
import Data.Maybe
import Control.Applicative

output1 :: IO Int
output1 = readFile "Day15Input.txt" <&> part1 . parse

output2 :: IO Int
output2 = readFile "Day15Input.txt" <&> part2 . wide . parse

parse :: [Char] -> (V.Vector Char, Int, (Int, Int), String)
parse ss = (\(a, b, c, y) -> (a, length $ takeWhile (== '#') ss, fromJust c, b)) (parser 0 ss)
    where parser y xs = case break (== '\n') xs of
            ([], '\n' : ys) -> (V.empty, ys, Nothing, y)
            (rs, '\n' : ys) -> let v  = V.fromList rs
                                   x  = fmap (,y) (V.elemIndex '@' v)
                                   v' = maybe v (\(x, y) -> v V.// [(x, '.')]) x                 
                            in (\(vs, b, c, d) -> (v' V.++ vs, b, x <|> c, d)) (parser (y + 1) ys)

wide :: (V.Vector Char, Int, (Int, Int), String) -> (V.Vector Char, Int, (Int, Int), String)
wide (grid, bound, (x, y), xs) = (V.foldr replace V.empty grid, bound * 2, (x * 2, y), xs)
        where replace '.' b = V.cons '.' (V.cons '.' b)
              replace '#' b = V.cons '#' (V.cons '#' b)
              replace 'O' b = V.cons '[' (V.cons ']' b)
              replace v   _ = error (show v)

part1 :: (V.Vector Char, Int, (Int, Int), String) -> Int
part1 (grid, bound, (x, y), []    ) = count 'O' grid bound
part1 (grid, bound, i,   '\n' : ds) = part1 (grid, bound, i, ds)
part1 (grid, bound, (x, y), d : ds) = initial
    where (dX, dY)    = delta d
          (nX, nY)    = (x + dX, y + dY)
          initial     = case grid V.! ((nY * bound) + nX) of
            '.' -> part1 (grid, bound, (nX, nY), ds)                         -- only move robot
            '#' -> part1 (grid, bound, (x, y), ds)                           -- stop robot
            'O' -> next (nX, nY)                                             -- search further                                   
          next (a, b) = case grid V.! ((b * bound) + a) of
            '.' -> let updates = [(nY * bound + nX, '.'), (b * bound + a, 'O')]
                   in part1 (grid V.// updates, bound, (nX, nY), ds)         -- found open spot
            '#' -> part1 (grid, bound, (x, y), ds)                           -- stop robot
            'O' -> next (a + dX, b + dY)                                     -- search further

part2 :: (V.Vector Char, Int, (Int, Int), String) -> Int
part2 (grid, bound, (x, y), []    ) = count '[' grid bound
part2 (grid, bound, i,   '\n' : ds) = part2 (grid, bound, i, ds)
part2 (grid, bound, (x, y), d : ds) = case initial of
    Nothing -> part2 (grid, bound, (x ,  y), ds)
    Just [] -> part2 (grid, bound, (nX, nY), ds)
    Just xs -> part2 (change (grid, bound) (dX, dY) xs, bound, (nX, nY), ds)
  where (nX, nY)    = bimap (x+) (y+) (delta d)
        (dX, dY)    = delta d
        initial     = case (d, grid V.! ((nY * bound) + nX)) of
            (_  , '.') -> Just []
            (_  , '#') -> Nothing
            ('>', '[') -> horizontal (nX, nY)
            ('<', ']') -> horizontal (nX, nY)
            ('^', '[') -> vertical [((nX, nY), '['), ((nX + 1, nY), ']')]
            ('^', ']') -> vertical [((nX, nY), ']'), ((nX - 1, nY), '[')]
            ('v', '[') -> vertical [((nX, nY), '['), ((nX + 1, nY), ']')]
            ('v', ']') -> vertical [((nX, nY), ']'), ((nX - 1, nY), '[')]          
        horizontal (a, b) = case grid V.! ((b * bound) + a) of
            '.' -> Just []
            '#' -> Nothing
            ']' -> fmap (((a, b), ']') :) (horizontal (a + dX, b + dY))
            '[' -> fmap (((a, b), '[') :) (horizontal (a + dX, b + dY))
        vertical xs = case foldr (liftA2 (++) . extend . bimap (+ dX) (+ dY) . fst) (Just []) xs of
            Nothing -> Nothing
            Just [] -> Just xs
            Just ys -> fmap (xs ++) (vertical ys)
        extend (a, b) = case grid V.! ((b * bound) + a) of
            '.' -> Just []
            '#' -> Nothing
            '[' -> Just [((a, b), '['), ((a + 1, b), ']')]
            ']' -> Just [((a, b), ']'), ((a - 1, b), '[')]
                
change :: (V.Vector Char, Int) -> (Int, Int) -> [((Int, Int), Char)] -> V.Vector Char
change (grid, bound) (x, y) xs = (grid V.// resets) V.// updates
    where resets  = map (\((a, b), c) -> ((b * bound) + a, '.')) xs
          updates = map (\((a, b), c) -> (((b + y) * bound) + (a + x), c)) xs

delta :: Char -> (Int, Int)
delta '<' = (-1, 0)
delta '>' = (1, 0)
delta '^' = (0, -1)
delta 'v' = (0, 1)

display :: V.Vector Char -> Int -> String
display grid width = V.ifoldr (\i a b -> if i `mod` width == (width - 1) then a : '\n' : b else a : b) [] grid

count :: Char -> V.Vector Char -> Int -> Int
count c grid width = V.ifoldr (\i a b -> (if a == c then cost i else 0) + b) 0 grid
    where cost i = ((i `div` width) * 100) + (i `mod` width)