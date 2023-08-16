module CollatzConjecture (collatz) where


next :: Integer -> Integer
next n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

helper :: Integer -> Integer -> Maybe Integer
helper step n
  | n <= 0 = Nothing
  | n == 1 = Just step
  | otherwise = helper (step + 1) (next n)

collatz :: Integer -> Maybe Integer
collatz = helper 0

