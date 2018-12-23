module Main where

import           System.IO
import           Control.Monad
import qualified Data.Set as Set

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print $ contents
