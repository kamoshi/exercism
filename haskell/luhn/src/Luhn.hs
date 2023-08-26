module Luhn (isValid) where

import Data.Char (isSpace, isNumber, digitToInt)


validate :: [Int] -> Bool
validate [] = False
validate [_] = False
validate ns = (0 ==) . (`rem` 10) . sum . mapOdd double . reverse $ ns
  where
    double x = let xx = x * 2 in if xx > 9 then xx - 9 else xx
    mapOdd f = zipWith ($) (cycle [id, f])

isValid :: String -> Bool
isValid n
  | isWellFormed n = validate . map digitToInt . filter isNumber $ n
  | otherwise = False
  where
    canBeParsed c = isSpace c || isNumber c
    isWellFormed = all canBeParsed
