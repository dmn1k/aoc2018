module Main where

import           System.IO
import           Control.Monad
import qualified Data.Set as Set

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print $ part1 contents
    print $ part2 contents

part1 :: String -> Int
part1 input = sum (parseNumbers input)

part2 :: String -> Int
part2 input = do
    let numbers = parseNumbers input
    findFirstDuplicate numbers

findFirstDuplicate :: [Int] -> Int
findFirstDuplicate xs = dup (cycle xs) Set.empty 0
    where dup (x:xs) s currFreq = do
                                let nextFreq = currFreq + x
                                if Set.member nextFreq s
                                then nextFreq
                                else dup xs (Set.insert nextFreq s) nextFreq

parseNumbers :: String -> [Int]
parseNumbers input = map (readInt . removePlus) (lines input)

readInt :: String -> Int
readInt = read

removePlus :: String -> String
removePlus xs = [x | x <- xs, x /= '+']
