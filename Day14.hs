module Day14 where

import Data.Functor ( (<&>) )

import Data.Array
import Data.Text qualified as T


output1 :: IO Int
output1 = readFile "Day14Input.txt" <&> part1 100 . parse

output2 :: IO ()
output2 = do
    input <- readFile "Day14Input.txt"
    rep (0, parse input)
    return ()

rep :: (Int, [((Int, Int), (Int, Int))]) -> IO b
rep (a, b) = do 
    let (n, xs) = part2 (a, b)
    putStrLn (display (101, 103) (map fst xs))
    print n
    x <- getLine
    rep (n, xs)

part1 :: Int -> [((Int, Int), (Int, Int))] -> Int
part1 n xs = let (a, b, c, d) = foldr f (0, 0, 0, 0) xs in a * b * c * d
    where f v (a, b, c, d) = case quadrant (50, 51) (move (101, 103) n v) of
                                1 -> (a + 1, b, c, d)
                                2 -> (a, b + 1, c, d)
                                3 -> (a, b, c + 1, d)
                                4 -> (a, b, c, d + 1)
                                0 -> (a, b, c, d)

part2 :: (Int, [((Int, Int), (Int, Int))]) -> (Int, [((Int, Int), (Int, Int))])           
part2 (n, xs) | n > 101*103        = error "infinite recursion!"
              | difference < 45    = (n + 1, updated)
              | otherwise          = part2 (n + 1, updated)
    where updated      = map (\v@(a, b) -> (move (101, 103) 1 v, b)) xs
          difference   = foldr (\((x, y), _) b -> abs(x - 50) + abs(y - 51) + b) 0 updated `div` 500

display :: (Int, Int) -> [(Int, Int)] -> String
display (bX, bY) xs = foldr (\y b -> row y ++ "\n" ++ b) [] [0..(bY - 1)]    
    where row y = map (\x -> if (x, y) `elem` xs then 'X' else '.') [0.. (bX - 1)]

parse :: String -> [((Int, Int), (Int, Int))]
parse r0 = ((read pX, read pY), (read vX, read vY)) : if null r4 then [] else parse (drop 1 r4)
    where (pX, r1) = break (== ',')  (drop 2 r0)
          (pY, r2) = break (== ' ')  (drop 1 r1)
          (vX, r3) = break (== ',')  (drop 3 r2)
          (vY, r4) = break (== '\n') (drop 1 r3)

move :: (Int, Int) -> Int -> ((Int, Int), (Int, Int)) -> (Int, Int)
move (bX, bY) count ((pX, pY), (vX, vY)) = ((pX + vX * count) `mod` bX, (pY + vY * count) `mod` bY)

quadrant :: (Int, Int) -> (Int, Int) -> Int
quadrant (bX, bY) (x, y) | x < bX && y < bY = 1
                         | x > bX && y < bY = 2
                         | x > bX && y > bY = 3
                         | x < bX && y > bY = 4
                         | otherwise        = 0

