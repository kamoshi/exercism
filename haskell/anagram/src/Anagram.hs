module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (nub)


check :: String -> String -> Bool
check w1 w2 = l1 /= l2 && all equal chars
  where
    l1 = map toLower w1
    l2 = map toLower w2
    chars = nub l1
    count c = length . filter (c==)
    equal c = count c l1 == count c l2

anagramsFor :: String -> [String] -> [String]
anagramsFor word = filter (check word) . filter lengths
  where
    lengths other = length word == length other

