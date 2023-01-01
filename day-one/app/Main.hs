module Main (main) where

import Data.List

main :: IO ()
main = do
  input <- getContents
  let xs = foldr f [] (lines input)
  print (partOne xs)
  print (partTwo xs)

f :: String -> [Int] -> [Int]
f [] [] = [] 
f [] (y:ys) = 0:y:ys
f x (y:ys) = ((read x :: Int) + y):ys
f x [] = [read x :: Int]

partOne :: [Int] -> Int
partOne = maximum

partTwo :: [Int] -> Int
partTwo xs = sum (take 3 (sortBy (flip compare) xs))