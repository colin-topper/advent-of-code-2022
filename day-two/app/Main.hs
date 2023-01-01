module Main (main) where

main :: IO ()
main = do 
    input <- getContents
    let rounds = map words (lines input)
    print $! sum (map partOneScore rounds)
    print $! sum (map partTwoScore rounds)

partOneScore :: [String] -> Int
partOneScore ("A":"X":_) = 3 + 1 
partOneScore ("B":"X":_) = 0 + 1
partOneScore ("C":"X":_) = 6 + 1
partOneScore ("A":"Y":_) = 6 + 2 
partOneScore ("B":"Y":_) = 3 + 2
partOneScore ("C":"Y":_) = 0 + 2
partOneScore ("A":"Z":_) = 0 + 3 
partOneScore ("B":"Z":_) = 6 + 3
partOneScore ("C":"Z":_) = 3 + 3
partOneScore _ = 0

partTwoScore :: [String] -> Int
partTwoScore ("A":"X":_) = 3 + 0 
partTwoScore ("B":"X":_) = 1 + 0
partTwoScore ("C":"X":_) = 2 + 0
partTwoScore ("A":"Y":_) = 1 + 3 
partTwoScore ("B":"Y":_) = 2 + 3
partTwoScore ("C":"Y":_) = 3 + 3
partTwoScore ("A":"Z":_) = 2 + 6 
partTwoScore ("B":"Z":_) = 3 + 6
partTwoScore ("C":"Z":_) = 1 + 6
partTwoScore _ = 0