module Solutions
    ( solution1a
    , solution1b
    , module Day2
    , module Day3
    , module Day4
    ) where

import Day2
import Day3
import Day4

solution1a :: String -> String
solution1a = show . solve1a . parse1

solution1b :: String -> String
solution1b = show . solve1b . parse1

parse1 :: String -> [Integer]
parse1 = map read . lines

solve1a :: [Integer] -> Int
solve1a is = length . filter (uncurry (<)) . zip is $ tail is

solve1b :: [Integer] -> Int
solve1b is = solve1a . zipWith3 (\a b c -> a + b + c) is (tail is) $ (tail . tail $ is)
