module Main where

import           System.IO
import           Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import           Data.List

type CharOccurences = MultiSet Char
data ChecksumAttributes = Attr Bool Bool

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let entries = lines contents
    print $ toChecksum (map (toChecksumAttributes . groupByChar) entries)
    let similarPair = findSimilarPair (pairs entries)
    print $ removeDifferences (fst similarPair) (snd similarPair)

toChecksum :: [ChecksumAttributes] -> Int
toChecksum attrs = calcChecksum attrs 0 0
    where calcChecksum [] twos threes = twos * threes
          calcChecksum (x:xs) twos threes = case x of
                                                Attr True True -> calcChecksum xs (twos + 1) (threes + 1)
                                                Attr True False -> calcChecksum xs (twos + 1) (threes)
                                                Attr False True -> calcChecksum xs (twos) (threes + 1)
                                                Attr False False -> calcChecksum xs (twos) (threes)


toChecksumAttributes :: CharOccurences -> ChecksumAttributes
toChecksumAttributes occ = do
    let counts = [ ct | (ch, ct) <- MultiSet.toOccurList occ]
    Attr (2 `elem` counts) (3 `elem` counts)

groupByChar :: String -> CharOccurences
groupByChar input = doGroup MultiSet.empty input
    where doGroup set [] = set
          doGroup set (x:xs) = doGroup (MultiSet.insert x set) xs

removeDifferences :: String -> String -> String
removeDifferences a b = removeSpace (zipWith toRes a b)
    where toRes x y = if x == y then x else ' '

removeSpace :: String -> String
removeSpace xs = [x | x <- xs, x /= ' ']

findSimilarPair :: [(String, String)] -> (String, String)
findSimilarPair (x:xs) = if diff <= 1 then x else findSimilarPair xs
    where diff = countDifferences (fst x) (snd x)

countDifferences :: String -> String -> Int
countDifferences a b = sum (zipWith cmp a b)
    where cmp x y = if x == y then 0 else 1

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]
