module Main where

import Lib (findWords, grid, languages, outputGrid)

main :: IO ()
main = do
    outputGrid grid
    putStrLn $ unlines $ findWords grid languages
