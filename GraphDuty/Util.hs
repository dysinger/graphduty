module GraphDuty.Util where

import Data.Char

camelToSnake :: String -> String
camelToSnake [] = []
camelToSnake (x:xs) = toLower x : go xs
  where
    go [] = []
    go (y:ys)
      | isUpper y = '_' : toLower y : go ys
      | otherwise = y : go ys
