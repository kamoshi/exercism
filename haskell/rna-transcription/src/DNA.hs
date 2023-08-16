module DNA (toRNA) where


toRNA :: String -> Either Char String
toRNA "" = Right ""
toRNA (x:xs) = (++) <$> result <*> toRNA xs
  where
    result = case x of
      'G' -> Right "C"
      'C' -> Right "G"
      'T' -> Right "A"
      'A' -> Right "U"
      other -> Left other

