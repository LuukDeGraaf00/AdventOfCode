module Day3 where

import Data.List
import Data.Char
import Data.Maybe
import Data.Functor

-- | Extract output.
output1 :: IO Int
output1 = readFile "Day3Input.txt" <&> parse

-- | Extract output.
output2 :: IO Int
output2 = readFile "Day3Input.txt" <&> parseOn

-- | Part 1.
parse :: String -> Int
parse ('m' : 'u' : 'l' : '(' : xs) = maybe (parse xs) (\(n, x) -> n + parse x) (mul xs)
parse (x : xs)                     = 0 + parse xs 
parse []                           = 0

-- | Part 2, when enabled.
parseOn :: String -> Int
parseOn ('m' : 'u' : 'l' : '(' : xs)        = maybe (parseOn xs) (\(n, x) -> n + parseOn x) (mul xs)
parseOn ('d' : 'o' : 'n' : '\'' : 't' : xs) = parseOff xs
parseOn (x : xs)                            = 0 + parseOn xs 
parseOn []                                  = 0

-- | Part 2, when disabled.
parseOff :: String -> Int
parseOff ('d' : 'o' : '(' : ')' : xs) = parseOn  xs
parseOff (x : xs)                     = parseOff xs
parseOff []                           = 0

-- | Parsing of multiply statement.
mul :: String -> Maybe (Int, String)
mul x0 = if valid then Just (read v1 * read v3, x4) else Nothing
    where (v1, x1) = span isDigit x0
          (v2, x2) = fromMaybe ('_', []) (uncons x1)
          (v3, x3) = span isDigit x2     
          (v4, x4) = fromMaybe ('_', []) (uncons x3)
          valid    = not (null v1) && not (null v3) && v2 == ',' && v4 == ')'
