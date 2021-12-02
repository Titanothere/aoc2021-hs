module Main where

import Solutions

main :: IO ()
main = do i1 <- readFile "inputfiles/input1"
          putStrLn $ solution1a i1
          putStrLn $ solution1b i1
