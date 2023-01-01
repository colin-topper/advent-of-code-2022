module Main (main) where

import qualified Data.List.Split

main :: IO ()
main = do 
    input <- getContents
    print $ partOne (lines input)
    print $ partTwo (lines input)

parse :: String -> ((Int, Int), (Int, Int))
parse x = do
    let [a, b, c, d] = Data.List.Split.splitOneOf ",-" x
    ((read a, read b),(read c, read d))

encompass :: ((Int, Int), (Int, Int)) -> Bool
encompass ((a,b),(x,y)) = ((a <= x) && (b >= y)) || ((x <= a) && (y >= b))

overlap :: ((Int, Int), (Int, Int)) -> Bool
overlap ((a,b),(x,y)) =
    ((a <= x) && (b >= x)) ||
    ((a <= y) && (b >= y)) ||
    ((x <= a) && (y >= a)) ||
    ((x <= b) && (y >= b))

partOne :: [String] -> Int
partOne xs = length (filter (encompass . parse) xs)

partTwo :: [String] -> Int
partTwo xs = length (filter (overlap . parse) xs)