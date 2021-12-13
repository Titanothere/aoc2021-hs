module Day4
  (solution4a, solution4b)
  where

import Data.Char
import Data.List
import Text.ParserCombinators.ReadP

solution4a :: String -> String
solution4a = show . uncurry score . uncurry playTillFirst . parse4

solution4b :: String -> String
solution4b = show . uncurry score . uncurry playTillLast . parse4

parse4 :: String -> ([Bingo], Draws)
parse4 = fst . head . readP_to_S readInput

readInput :: ReadP ([Bingo], Draws)
readInput = do draws <- readDraws
               boards <- many1 readBingo
               skipSpaces
               eof
               return (boards, draws)

readBingo :: ReadP Bingo
readBingo = do rows <- count 5 (skipSpaces >> readRow)
               return $ rows ++ transpose rows

readRow :: ReadP [Int]
readRow = count 5 (skipSpaces >> readPositiveInt)

readDraws :: ReadP Draws
readDraws = sepBy1 readPositiveInt (char ',') 

readPositiveInt :: ReadP Int
readPositiveInt = read <$> munch1 isNumber

type Bingo = [[Int]]
type Draws = [Int]

wins :: Draws -> Bingo -> Bool
wins draws = any $ all (`elem` draws)

playTillFirst :: [Bingo] -> Draws -> (Bingo, Draws)
playTillFirst boards draws = (\(bs, ds) -> (head bs, ds)) . head . dropWhile (null . fst) 
                  $ [ (filter (wins draws') boards, draws') | draws' <- inits draws ]

playTillLast :: [Bingo] -> Draws -> (Bingo, Draws)
playTillLast boards draws = playTillFirst [lastBoard] draws
  where lastBoard = head . head . dropWhile ((/=1) . length)
                  $ [ filter (not . wins draws') boards | draws' <- inits draws ]

score :: Bingo -> Draws -> Int
score board draws = (* last draws) . sum . (\\ draws) . concat $ drop 5 board