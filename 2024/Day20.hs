module Day20 where

import Data.List
import Data.Functor ((<&>))
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (isJust, isNothing, fromJust, mapMaybe)

type Result = ((Int, Int), (Int, Int), S.Set (Int, Int))

output1 :: IO Int
output1 = readFile "Day20Input.txt" <&> length . solve (-100) 2 . parse (0, 0) (undefined, undefined, S.empty)

output2 :: IO Int
output2 = readFile "Day20Input.txt" <&> length . solve (-100) 20 . parse (0, 0) (undefined, undefined, S.empty)

solve :: Int -> Int -> (a, (Int, Int), S.Set (Int, Int)) -> [Int]
solve save jump (start, end, obstacles) = cheats
    where distances     = grid end obstacles
          cheats        = M.foldrWithKey (\k a b -> mapMaybe (comp a) (nears jump k) ++ b) [] distances
          comp c (j, p) = M.lookup p distances >>= (\i -> if i <= save then Just i else Nothing) . ((c + j) -)

grid :: (Int, Int) -> S.Set (Int, Int) -> M.Map (Int, Int) Int
grid start invalid = bfs 1 [start] invalid (M.singleton start 0)
    where bfs n current invalid m = if null current then m else 
            let options = filter valid (nub (concatMap near1 current))
                valid v = v `notElem` invalid && isNothing (M.lookup v m)
            in bfs (n+1) options invalid (foldr (`M.insert` n) m options)

near1 :: (Int, Int) -> [(Int, Int)]
near1 (x, y) = [(x - 1, y), (x, y - 1), 
                (x + 1, y), (x, y + 1)]

nears :: Int -> (Int, Int) -> [(Int, (Int, Int))]
nears n (x, y) = [(abs dx + abs dy, (x + dx, y + dy)) | dx <- [-n..n], dy <- [-n..n], let dist = abs dx + abs dy in dist > 1 && dist <= n]

parse :: (Int, Int) -> Result -> String -> Result
parse _      r         []          = r
parse (x, y) r         ('\n' : as) = parse (0, y + 1) r as
parse (x, y) (_, e, r) ('S'  : as) = parse (x + 1, y) ((x, y), e, r) as
parse (x, y) (s, _, r) ('E'  : as) = parse (x + 1, y) (s, (x, y), r) as
parse (x, y) r         ('.'  : as) = parse (x + 1, y) r as
parse (x, y) (s, e, r) ('#'  : as) = parse (x + 1, y) (s, e, S.insert (x, y) r) as

