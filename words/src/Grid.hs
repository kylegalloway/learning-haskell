module Grid ( diagonalize
           , findWord
           , findWords
           , findWordInLine
           , formatGrid
           , getLines
           , gridReverse
           , outputGrid
           , skew
            ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)
import Types (Grid)

findWord :: Grid -> String -> Maybe String
findWord grid word =
    let lines = getLines grid
        found = or $ map (findWordInLine word) lines
    in if found then Just word else Nothing

findWords :: Grid -> [String] -> [String]
findWords grid words =
    let foundWords = map (findWord grid) words
    in catMaybes foundWords

formatGrid :: Grid -> String
formatGrid = unlines

outputGrid :: Grid -> IO ()
outputGrid = putStrLn . formatGrid

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf

getLines :: Grid -> Grid
getLines grid =
    let horizontal = grid
        vertical = transpose grid
        diagonal1 = diagonalize horizontal
        diagonal2 = diagonalize $ gridReverse horizontal
        lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
    in lines ++ gridReverse lines

gridReverse :: Grid -> Grid
gridReverse = map reverse

diagonalize :: Grid -> Grid
diagonalize = transpose . skew

skew :: Grid -> Grid
skew [] = []
skew (l:ls) = l : skew (map indent ls)
    where indent line = '_' : line