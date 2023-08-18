module DNA (nucleotideCounts, Nucleotide(..)) where

import qualified Data.Map as M


data Nucleotide = A | C | G | T
  deriving (Eq, Ord, Show)

type Nucleotides = M.Map Nucleotide Int
type Result = Either String Nucleotides


count :: Result -> String -> Result
count nucleotides [] = nucleotides
count nucleotides (c:cs) = count next cs
  where
    value = case c of
      'A' -> Right A
      'C' -> Right C
      'G' -> Right G
      'T' -> Right T
      _ -> Left "error"
    next = M.insertWith (+) <$> value <*> pure 1 <*> nucleotides

nucleotideCounts :: String -> Result
nucleotideCounts = count $ Right M.empty

