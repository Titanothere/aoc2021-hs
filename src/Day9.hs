module Day9
  (solution9a, solution9b)
  where

import Data.List
import Data.Char

import Utils.Parsing

solution9a :: String -> String
solution9a = show . sum . map riskLevel . filter isLowPoint . mByNWindow 3 3 . parse9

solution9b :: String -> String
solution9b = const "Not yet solved"

parse9 :: String -> [[Int]]
parse9 = fst . head . readP_to_S readInput

readInput :: ReadP [[Int]]
readInput = do
  lines <- many1 (skipSpaces >> readLine)
  skipSpaces >> eof
  return . transpose
         . map ((9:) . (++[9]))
         . transpose
         . map ((9:) . (++[9]))
         $ lines

readLine :: ReadP [Int]
readLine = map (read . return) <$> munch1 isDigit

type Window a = [[a]]

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n xs = zipWith const (map (take n) (tails xs)) (drop (n-1) xs)

mByNWindow :: Int -> Int -> [[a]] -> [Window a]
mByNWindow m n = concat . map (map transpose . slidingWindow n . transpose) . slidingWindow m

isLowPoint :: Window Int -> Bool
isLowPoint [[_, a, _]
           ,[b, x, c]
           ,[_, d, _]] = a > x && b > x && c > x && d > x

riskLevel :: Window Int -> Int
riskLevel [[_, _, _]
          ,[_, x, _]
          ,[_, _, _]] = x + 1