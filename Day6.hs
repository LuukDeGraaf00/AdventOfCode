module Day6 where

import Data.Functor
import Data.Bifunctor

import Data.Vector qualified as V
import Data.Set qualified as S

import Debug.Trace as T
import Data.List (nub)

type Guard  = ((Int, Int), Char)
type Grid   = V.Vector (V.Vector Char)


-- | Extract output.
output1 :: IO Int
output1 = readFile "Day6Input.txt" <&> (\g -> length $ next g S.empty (guard g)) . parse 

-- | Extract output.
output2 :: IO Int
output2 = readFile "Day6Input.txt" <&> (\g -> let gu = guard g in foldr (\i r -> if loop g i [] gu then 1 + r else r) 0 (next g mempty gu)) . parse 

-- | Next step
next :: Grid -> S.Set (Int, Int) -> Guard -> S.Set (Int, Int)
next grid path guard = let i@(y, x) = step guard in case grid V.!? y >>= (V.!? x) of
    Nothing  -> S.insert i path
    Just '#' -> next grid path (second rotate guard) 
    Just _   -> next grid (S.insert i path) (i, snd guard)

-- | Determines if path loops.
loop :: Grid -> (Int, Int) -> [Guard] -> Guard -> Bool
loop grid obstacle path guard = let i@(y, x) = step guard in case if i == obstacle then Just '#' else grid V.!? y >>= (V.!? x) of
    Nothing  -> False
    Just '#' -> guard `elem` path || loop grid obstacle (guard : path) (second rotate guard) 
    Just _   -> loop grid obstacle path (i, snd guard)

-- | Step in one direction
step :: Guard -> (Int, Int)
step ((y, x), '^') = (y - 1, x)
step ((y, x), '<') = (y, x - 1)
step ((y, x), '>') = (y, x + 1)
step ((y, x), 'v') = (y + 1, x)

-- | Rotate character.
rotate :: Char -> Char
rotate '^' = '>'
rotate '<' = '^'
rotate '>' = 'v'
rotate 'v' = '<'

-- | Determine initial guard location.
guard :: Grid -> Guard
guard = V.head . V.ifoldr (\y a r -> V.imapMaybe (isGuard y) a V.++ r) mempty

-- | is guard
isGuard :: Int -> Int -> Char -> Maybe Guard
isGuard y x '^' = Just ((y, x), '^')
isGuard y x '<' = Just ((y, x), '<')
isGuard y x '>' = Just ((y, x), '>')
isGuard y x 'v' = Just ((y, x), 'v')
isGuard _ _ _   = Nothing

-- | Parse map into vector.
parse :: String -> Grid
parse xs = V.fromList (map V.fromList (lines xs))


