module Day8
  (solution8a, solution8b)
  where

import Data.List

import Utils.Parsing

import Data.Maybe

solution8a :: String -> String
solution8a = show . length
           . filter (\l -> l == 2 || l == 3 || l == 4 || l == 7) . map length
           . concat . map snd . parse8

solution8b :: String -> String
solution8b = show . sum . map solveb . parse8

solveb :: ([Pattern], Value) -> Int
solveb (patterns, value) = read . concat . map show $ digits
  where
    [one]   = filter ((== 2) . length) patterns
    [four]  = filter ((== 4) . length) patterns
    [seven] = filter ((== 3) . length) patterns
    [eight] = filter ((== 7) . length) patterns
    [nine]  = [ p | p <- length6, four `subset` p ]
    [zero]  = [ p | p <- length6, one  `subset` p, p /= nine ]
    [six]   = [ p | p <- length6, p /= nine, p /= zero]
    [three] = [ p | p <- length5, one `subset` p ]
    [five]  = [ p | p <- length5, p `subset` six ]
    [two]   = [ p | p <- length5, p /= three, p /= five ]
    length5 = [ p | p <- patterns, length p == 5 ]
    length6 = [ p | p <- patterns, length p == 6 ]
    mapping = [zero, one, two, three, four, five, six, seven, eight, nine]
    digits  = map (fromJust . (`elemIndex` mapping)) value

subset :: String -> String -> Bool
subset "" s = True
subset _ "" = False
subset (c1:s1) (c2:s2) | c1 < c2  = False
                       | c1 == c2 = subset s1 s2
                       | c1 >  c2 = subset (c1:s1) s2 

type Line = ([Pattern], Value)
type Pattern = String
type Value = [Pattern]

parse8 :: String -> [Line]
parse8 = fst . head . readP_to_S readInput

readInput :: ReadP [Line]
readInput = do
  ls <- many1 (skipSpaces >> readLine)
  skipSpaces
  eof
  return ls

readLine :: ReadP Line
readLine = do
  patterns <- count 10 (skipSpaces >> readPattern)
  skipSpaces
  char '|'
  skipSpaces
  values <- count 4 (skipSpaces >> readPattern)
  return (patterns, values)

readPattern :: ReadP Pattern
readPattern = sort <$> munch1 (`elem` "abcdefg")