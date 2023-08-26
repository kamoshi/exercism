module CryptoSquare (encode) where

import Data.Char (isAlphaNum,toLower)
import Data.List (unfoldr)


normalize :: String -> String
normalize = map toLower . filter isAlphaNum

findSide :: String -> Int
findSide = ceiling . sqrt . fromIntegral . length

chunks :: Int -> String -> [String]
chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)

toSquare :: String -> [String]
toSquare s = filter (any isAlphaNum) $ chunks c padded
  where
    c = findSide s
    padded = s ++ [' ' | _ <- [1..c - (length s `mod` c)]]

fromSquare :: [String] -> String
fromSquare [] = ""
fromSquare ss =  unwords $ map (($ ss) . takeIndex) [0..c-1]
  where
    c = length . head $ ss
    takeIndex n = map (!!n)

encode :: String -> String
encode = fromSquare . toSquare . normalize

