module Day7
  ( solution7a, solution7b )
  where

import Data.List (sort)

import Utils.Parsing
import Text.ParserCombinators.ReadP

import Debug.Trace

solution7a :: String -> String
solution7a s = show . sum $ map (abs . subtract median) ints
  where ints   = sort $ parse7 s
        median = ints !! ((length ints `div` 2) - 1)

solution7b :: String -> String
solution7b s = show cost
  where ints   = sort $ parse7 s
        cost = minimum [ sum $ map (triangulars . abs . subtract x) ints | x <- [minimum ints..maximum ints]]

parse7 :: String -> [Int]
parse7 = fst . head . readP_to_S readInput

readInput :: ReadP [Int]
readInput = do is <- sepBy1 readPositiveInt (char ',')
               skipSpaces >> eof
               return is

triangulars :: Int -> Int
triangulars n = n*(n+1) `div` 2