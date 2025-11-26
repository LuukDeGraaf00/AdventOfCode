module Day19 where
import Data.Functor
import Data.List
import Data.Maybe (mapMaybe, isJust, fromJust, fromMaybe)
import Data.Bifunctor (first)
import qualified Data.Map as M

output1 :: IO Int
output1 = readFile "Day19Input.txt" <&> length . filter (>0) . results . parse

output2 :: IO Int
output2 = readFile "Day19Input.txt" <&> sum . results . parse

parse :: String -> ([String], [String])
parse xs = (concatMap delimiter options, tail values)
    where (options, values) = break null (lines xs)

delimiter :: String -> [String]
delimiter s = if null result then [] else w : delimiter next
    where invalid a = a == ' ' || a == '\n' || a == ','
          result    = dropWhile invalid s 
          (w, next) = break invalid result

results :: ([String], [String]) -> [Int]
results (options, values) = fst $ foldr f ([], M.fromList [([], 1)]) values
    where f string (ns, cache) = let (n, cache') = solve options string cache in (n : ns, cache')

solve :: [String] -> String -> M.Map String Int -> (Int, M.Map String Int)
solve options string cache = maybe (count, M.insert string count c) (, cache) (M.lookup string cache)
    where possible   = mapMaybe (`stripPrefix` string) options
          (count, c) = foldr (\a (n, b) -> first (+n) (solve options a b)) (0, cache) possible