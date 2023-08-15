module Pangram (isPangram) where
import Data.List (nub)
import Data.Char (toLower, isAsciiLower)

isPangram :: String -> Bool
isPangram = (26==) . length . nub . filter isAsciiLower . map toLower

