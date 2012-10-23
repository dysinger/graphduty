{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes      #-}

import Data.List
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = $(defaultMainGenerator)

--- QuickCheck

prop_sort1 :: [Int] -> Bool
prop_sort1 xs = sort xs == sortBy compare xs
  where _types = (xs :: [Int])

prop_sort2 :: [Int] -> Property
prop_sort2 xs =
        (not (null xs)) ==>
        (head (sort xs) == minimum xs)
  where _types = (xs :: [Int])

prop_sort3 :: [Int] -> Property
prop_sort3 xs = (not (null xs)) ==>
        last (sort xs) == maximum xs
  where _types = (xs :: [Int])

prop_sort4 :: [Int] -> [Int] -> Property
prop_sort4 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (head (sort (xs ++ ys)) == min (minimum xs) (minimum ys))
  where _types = (xs :: [Int], ys :: [Int])

prop_sort5 :: [Int] -> [Int] -> Property
prop_sort5 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (head (sort (xs ++ ys)) == max (maximum xs) (maximum ys))
  where _types = (xs :: [Int], ys :: [Int])

prop_sort6 :: [Int] -> [Int] -> Property
prop_sort6 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (last (sort (xs ++ ys)) == max (maximum xs) (maximum ys))
  where _types = (xs :: [Int], ys :: [Int])

-- HUnit

test_sort7 :: Assertion
test_sort7 = sort [8, 7, 2, 5, 4, 9, 6, 1, 0, 3] @?= ([0..9] :: [Integer])

test_sort8 :: forall t. t
test_sort8 = error "This test deliberately contains a user error"
