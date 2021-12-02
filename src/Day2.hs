module Day2 (solution2a, solution2b) where

import Control.Monad.State
import Debug.Trace

solution2a :: String -> String
solution2a = show . (\(h, d) -> h*d)
           . flip execState initialStateA . sequence_ . map interpA . parse2

solution2b :: String -> String
solution2b = show . (\(h, d, a) -> h*d)
           . flip execState initialStateB . sequence_ . map interpB . parse2

parse2 :: String -> [Sub]
parse2 = map (parseSub . words) . lines

parseSub :: [String] -> Sub
parseSub [instruction, n] = case instruction of
                              "forward" -> Forward (read n)
                              "down"    -> Down    (read n)
                              "up"      -> Up      (read n)


type Depth = Integer
type HPos  = Integer
type Aim   = Integer

data Sub = Forward Integer
         | Down    Integer
         | Up      Integer

type SubA = State (HPos, Depth) ()
type SubB = State (HPos, Depth, Aim) ()

initialStateA = (0, 0)
initialStateB = (0, 0, 0)


interpA :: Sub -> SubA
interpA (Forward n) = do (h, d) <- get
                         put (h+n, d)

interpA (Down n)    = do (h, d) <- get
                         put (h, d+n) 

interpA (Up n)      = do (h, d) <- get
                         put (h, max 0 (d-n))

interpB :: Sub -> SubB
interpB (Forward n) = do (h, d, a) <- get
                         put (h+n, (d + n*a), a)

interpB (Down n)    = do (h, d, a) <- get
                         put (h, d, a+n)

interpB (Up n)      = do (h, d, a) <- get
                         put (h, d, a-n)