module Day12 where


import qualified Data.Array.Unboxed as A
import Data.Set qualified as S
import Data.Functor
import Data.List


output1 :: IO Int
output1 = readFile "Day12Input.txt" <&> sum . map cost . regions . parser

output2 :: IO Int
output2 = readFile "Day12Input.txt" <&> sum . map cost2 . regions . parser

parser :: String -> A.Array (Int, Int) Char
parser file = let ls = lines file in A.listArray ((1, 1), (length ls, length $ head ls)) $ concat ls

region :: A.Array (Int, Int) Char -> (Int, Int) -> S.Set (Int, Int)
region grid pos = grow S.empty (S.singleton pos) 
    where
        remove c i = filter (\i -> (i `S.notMember` c) && inbounds i && same i) (neighbors i)
        char       = grid A.! pos 
        same pos   = grid A.! pos == char
        inbounds   = A.inRange $ A.bounds grid 
        
        grow :: S.Set (Int, Int) -> S.Set (Int, Int) -> S.Set (Int, Int) 
        grow current boundary = if S.null boundary 
            then current 
            else grow (S.union current boundary) (S.fromList $ concatMap (remove current) boundary)


regions :: A.Array (Int, Int) Char ->  [S.Set (Int, Int)]
regions grid = grow [] (S.fromList  (A.indices grid))
    where       
        grow :: [S.Set (Int, Int)] -> S.Set (Int, Int) -> [S.Set (Int, Int)]
        grow found left = if S.null left 
            then found 
            else let new = region grid (S.elemAt 0 left) in grow (new : found) (S.difference left new) 

cost :: S.Set (Int, Int) -> Int
cost xs = length xs * S.foldr ((+) . foldr (\i b -> fromEnum (S.notMember i xs) + b) 0 . neighbors) 0 xs
        
cost2 :: S.Set (Int, Int) -> Int
cost2 xs = length xs * sum (map (\(y, x) -> ((length .) . filter) (\(a, b, c) -> 
                            all (`S.notMember` xs) [a, b] || 
                            all (`S.member`    xs) [a, b] && (c `S.notMember` xs))             
                            [((y + dy, x), (y, x + dx), (y + dy, x + dx)) | dy <- [-1, 1], dx <- [-1,1]]) (S.toList xs))


neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (y, x) = [(y, x-1), (y-1, x), (y, x+1), (y+1, x)]