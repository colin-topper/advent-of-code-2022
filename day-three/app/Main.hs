module Main (main) where

import qualified Data.List
import Data.Char

main :: IO ()
main = do
    input <- getContents
    let rucksacks = lines input
    print (partOne rucksacks)
    print (partTwo rucksacks)

splitCompartments :: String -> (String, String)
splitCompartments x = splitAt (length x `div` 2) x

findOverlap :: (String, String) -> String
findOverlap (a,b) = a `Data.List.intersect` b

toPriority :: String -> Int
toPriority (x:_) = if ord x > 96 then ord x - 96 else ord x - 38
toPriority [] = 0

partOne :: [String] -> Int
partOne rucksacks = do
    let compartments = map splitCompartments rucksacks
    let overlaps = map findOverlap compartments
    let priorities = map toPriority overlaps
    sum priorities

partTwo :: [String] -> Int
partTwo [] = 0
partTwo (x:y:z:xs) = toPriority (x `Data.List.intersect` y `Data.List.intersect` z) + partTwo xs
partTwo _ = 0