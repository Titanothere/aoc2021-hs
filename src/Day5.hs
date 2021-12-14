{-# LANGUAGE TupleSections #-}

module Day5
  (solution5a, solution5b)
  where

import Utils.Parsing
import Data.Char
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Text.ParserCombinators.ReadP

solution5a :: String -> String
solution5a = show . length . intersections . map pointSet . filter (xEqual ||| yEqual) . parse5

solution5b :: String -> String
solution5b = show . length . intersections . map pointSet . parse5

parse5 :: String -> [Line]
parse5 = fst . head . readP_to_S readInput

type Point = (Int, Int)
type Line  = (Point, Point)

readPoint :: ReadP Point
readPoint = (,) <$> readPositiveInt <*> (char ',' >> readPositiveInt)

skipArrow :: ReadP ()
skipArrow = string "->" >> return ()

readLine :: ReadP Line
readLine = (,) <$> readPoint <*> (skipSpaces >> skipArrow >> skipSpaces >> readPoint)

readInput :: ReadP [Line]
readInput = do ls <- many1 (skipSpaces >> readLine)
               skipSpaces >> eof
               return ls

xEqual :: Line -> Bool
xEqual ((x1, _), (x2, _)) = x1 == x2

yEqual :: Line -> Bool
yEqual ((_, y1), (_, y2)) = y1 == y2

pointSet :: Line -> Set Point
pointSet ((x1, y1), (x2, y2)) 
  | x1 == x2  = S.fromList [(x1, y) | y <- [min y1 y2..max y1 y2]]
  | y1 == y2  = S.fromList [(x, y1) | x <- [min x1 x2..max x1 x2]]
  | otherwise = if   compare x1 x2 == compare y1 y2
                then S.fromList . zip [min x1 x2..max x1 x2] $ [min y1 y2..max y1 y2]
                else S.fromList . zip [min x1 x2..max x1 x2] $ reverse [min y1 y2..max y1 y2]

intersections :: [Set Point] -> Set Point
intersections ls = foldr S.union S.empty [ S.intersection l1 l2 | (l1, l2) <- pairs ls ]

pairs :: [a] -> [(a, a)]
pairs []     = []
pairs (x:xs) = map (x,) xs ++ pairs xs

-- Or/And combinators for predicates
(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) p1 p2 a = p1 a || p2 a

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) p1 p2 a = p1 a && p2 a
