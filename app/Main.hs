module Main where

import Solutions

main :: IO ()
main = do i1 <- readFile "inputfiles/input1"
          putStrLn "Day1:"
          putStrLn $ "Part 1: " ++ solution1a i1
          putStrLn $ "Part 2: " ++ solution1b i1
          putStrLn ""

          i2 <- readFile "inputfiles/input2"
          putStrLn "Day2:"
          putStrLn $ "Part 1: " ++ solution2a i2
          putStrLn $ "Part 2: " ++ solution2b i2
          putStrLn ""

          i3 <- readFile "inputfiles/input3"
          putStrLn "Day3:"
          putStrLn $ "Part 1: " ++ solution3a i3
          putStrLn $ "Part 2: " ++ solution3b i3
          putStrLn ""

          i4 <- readFile "inputfiles/input4"
          putStrLn "Day4:"
          putStrLn $ "Part 1: " ++ solution4a i4
          putStrLn $ "Part 2: " ++ solution4b i4
          putStrLn ""

          i5 <- readFile "inputfiles/input5"
          putStrLn "Day5:"
          putStrLn $ "Part 1: " ++ solution5a i5
          putStrLn $ "Part 2: " ++ solution5b i5
          putStrLn ""
          
          i6 <- readFile "inputfiles/input6"
          putStrLn "Day6:"
          putStrLn $ "Part 1: " ++ solution6a i6
          putStrLn $ "Part 2: " ++ solution6b i6
          putStrLn ""
          
          i7 <- readFile "inputfiles/input7"
          putStrLn "Day7:"
          putStrLn $ "Part 1: " ++ solution7a i7
          putStrLn $ "Part 2: " ++ solution7b i7
