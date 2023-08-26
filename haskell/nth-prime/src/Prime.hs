module Prime (nth) where


primes :: [Integer]
primes = helper [2..]
  where
    sieve n = filter (\x -> (x `rem` n) /= 0)
    helper [] = []
    helper (n:ns) = n : helper (sieve n ns)

nth :: Int -> Maybe Integer
nth 0 = Nothing
nth n = Just $ primes !! (n - 1)
