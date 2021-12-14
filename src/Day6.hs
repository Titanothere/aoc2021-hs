module Day6 
  (solution6a, solution6b)
  where

import Utils.Parsing
import Data.Char
import Text.ParserCombinators.ReadP

solution6a :: String -> String
solution6a = show . length . head . drop 80 . iterate step . parse6

solution6b :: String -> String
solution6b = show . sum . head . drop 256 . iterate step2 . createState . parse6

parse6 :: String -> [Int]
parse6 = fst . head . readP_to_S readInput

readInput :: ReadP [Int]
readInput = do is <- readPositiveInt `sepBy1` char ','
               skipSpaces >> eof
               return is

step :: [Int] -> [Int]
step [] = []
step (0:ns) = 6 : 8 : step ns
step (n:ns) = n-1 : step ns

createState :: [Int] -> [Int]
createState is = [ length (filter (== n) is) | n <- [0..8] ]

step2 :: [Int] -> [Int]
step2 [a, b, c, d, e, f, g, h, i] = [b, c, d, e, f, g, a+h, i, a]