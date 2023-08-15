module Bob (responseFor) where
import Data.Char (isLower, isAsciiUpper, isSpace)
import Data.List (isSuffixOf)

responseFor :: String -> String
responseFor s
  | isQuestion s && isShouting s = "Calm down, I know what I'm doing!"
  | isQuestion s = "Sure."
  | isShouting s = "Whoa, chill out!"
  | isSilence s = "Fine. Be that way!"
  | otherwise = "Whatever."
  where
    chars = filter (not . isSpace)
    isSilence = null . chars
    isQuestion = ("?" `isSuffixOf`) . chars
    isShouting x = all ($ chars x) [not . any isLower, any isAsciiUpper]


