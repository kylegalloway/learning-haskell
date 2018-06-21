module Grid
    ( findWords
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)
import Consts (grid)
import Types (Grid)

diagonalize :: Grid -> Grid
diagonalize = transpose . skew

findWord :: Grid -> String -> Maybe String
findWord grid word =
    let lines = getLines grid
        found = or $ map (findWordInLine word) lines
    in if found then Just word else Nothing

findWords :: Grid -> [String] -> [String]
findWords grid words =
    let foundWords = map (findWord grid) words
    in catMaybes foundWords

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf

formatGrid :: Grid -> String
formatGrid = unlines

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

outputGrid :: Grid -> IO ()
outputGrid = putStrLn . formatGrid

skew :: Grid -> Grid
skew [] = []
skew (l:ls) = l : skew (map indent ls)
    where indent line = '_' : line