module GraphDuty.Util where

import Data.Char

camelToSnake :: String -> String
camelToSnake [] = []
camelToSnake (x:xs) = toLower x : snake xs
  where
    snake [] = []
    snake (y:ys)
      | isUpper y = '_' : toLower y : snake ys
      | otherwise = y : snake ys
