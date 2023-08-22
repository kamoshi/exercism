module SumOfMultiples (sumOfMultiples) where
import Data.List (unfoldr, nub)


mults :: Integer -> Integer -> [Integer]
mults _ 0 = []
mults l n = unfoldr helper 1
  where
    helper m
      | m * n >= l = Nothing
      | otherwise = Just (m * n, m + 1)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum . nub $ concatMap (mults limit) factors

