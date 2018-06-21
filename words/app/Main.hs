module Main where

import Lib

main :: IO ()
main = putStrLn $ unlines $ findWords grid languages
