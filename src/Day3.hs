module Day3 
  -- (solution3a, solution3b) 
  where

import Data.List

solution3a :: String -> String
solution3a input = show pc
  where
    ss = parse3 input
    gr = gammaRate ss
    er = invert gr
    pc = asNumber gr * asNumber er


solution3b :: String -> String
solution3b input = show pc
  where
    ss = parse3 input
    oR = oxygenRating ss
    cR = co2Rating ss
    pc = asNumber oR * asNumber cR

parse3 :: String -> [String]
parse3 = lines

asNumber :: String -> Int
asNumber [] = error "asNumber: empty bit string"
asNumber s  = go 0 s
  where go n []       = n
        go n ('0':s') = go (2*n)   s'
        go n ('1':s') = go (2*n+1) s'

gammaRate :: [String] -> String
gammaRate = map mostCommon . transpose

epsilonRate :: [String] -> String
epsilonRate = invert . gammaRate

oxygenRating :: [String] -> String
oxygenRating = go 0
  where go n [s] = s
        go n ss  = go (n+1) $ filter ((== c) . head . drop n) ss
          where c = mostCommon . head . drop n . transpose $ ss

co2Rating :: [String] -> String
co2Rating = go 0
  where go n [s] = s
        go n ss  = go (n+1) $ filter ((/= c) . head . drop n) ss
          where c = mostCommon . head . drop n . transpose $ ss

invert :: String -> String
invert [] = []
invert ('0':s) = '1':invert s
invert ('1':s) = '0':invert s

mostCommon :: String -> Char
mostCommon s = case compare ones zeroes of
                 LT -> '0'
                 _  -> '1'
  where ones   = length $ filter (=='1') s
        zeroes = length $ filter (=='0') s