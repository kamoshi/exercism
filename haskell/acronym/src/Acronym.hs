{-# LANGUAGE OverloadedStrings #-}
module Acronym (abbreviate) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (toUpper, isUpper)
import Control.Monad ((>=>))


split :: Text -> [Text]
split = T.splitOn "-" >=> T.splitOn " "

initials :: Text -> String
initials text = strategy text
  where
    getSingular = pure . toUpper . T.head
    getMultiple = T.unpack . T.filter isUpper
    strategy
      | T.all isUpper text = getSingular
      | T.any isUpper text = getMultiple
      | otherwise = getSingular

abbreviate :: String -> String
abbreviate = concatMap initials . filter ((0<) . T.length) . split . T.pack

