module Grid ( cell2char
            , completed
            , coordsGrid
            , diagonalize
            , fillInBlanks
            , findWord
            , findWordInCellLinePrefix
            , findWordInLine
            , findWords
            , formatGame
            , formatGameGrid
            , formatGrid
            , getLines
            , gridReverse
            , gridWithCoords
            , makeGame
            , makeRandomGrid
            , mapOverGrid
            , outputGrid
            , playGame
            , score
            , skew
            , totalWords
            , zipOverGrid
            , zipOverGridWith
            ) where

import Data.Char (toLower)
import Data.List (transpose)
import Data.Maybe (catMaybes, listToMaybe)
import System.Random
import qualified Data.Map as M

import Types (Cell (Cell, Indent), Game (Game, gameGrid, gameWords), Grid)

-- Public

completed :: Game -> Bool
completed game = score game == totalWords game

fillInBlanks :: RandomGen p => p -> Grid Char -> Grid Char
fillInBlanks gen grid =
    let rand = makeRandomGrid gen
        fill '_' r = r
        fill c _ = c
    in zipOverGridWith fill grid rand

formatGame :: Game -> String
formatGame game = formatGameGrid game
                  ++ "\n\n"
                  ++ (show $ score game)
                  ++ "/"
                  ++ (show $ totalWords game)

makeGame :: Grid Char -> [String] -> Game
makeGame grid wordsToFind =
    let gwc = gridWithCoords grid
        tuplify word = (word, Nothing)
        list = map tuplify wordsToFind
        dict = M.fromList list
    in Game gwc dict

playGame :: Game -> String -> Game
playGame game wordToFind | not $ M.member wordToFind (gameWords game) = game
playGame game wordToFind =
    let grid = gameGrid game
        foundWord = findWord grid wordToFind
    in case foundWord of
        Nothing -> game
        Just _ ->
            let dict = gameWords game
                newDict = M.insert wordToFind foundWord dict
            in game {gameWords = newDict }

-- Private

findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word =
    let gridLines = getLines grid
        foundWords = map (findWordInLine word) gridLines
    in listToMaybe $ catMaybes foundWords

findWords :: Grid Cell -> [String] -> [[Cell]]
findWords grid wordsToFind =
    let foundWords = map (findWord grid) wordsToFind
    in catMaybes foundWords

formatGameGrid :: Game -> String
formatGameGrid game =
    let grid = gameGrid game
        dict = gameWords game
        cellSet = concat . catMaybes . M.elems $ dict
        formatCell cell =
            let char = cell2char cell
            in if cell `elem` cellSet then char else toLower char
        charGrid = mapOverGrid formatCell grid
    in unlines charGrid

formatGrid :: Grid Cell -> String
formatGrid = unlines .  mapOverGrid cell2char

outputGrid :: Grid Cell -> IO ()
outputGrid = putStrLn . formatGrid

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords grid = zipOverGridWith Cell coordsGrid grid

totalWords :: Game -> Int
totalWords game = length . M.keys $ gameWords game

score :: Game -> Int
score game = length . catMaybes . M.elems $ gameWords game

makeRandomGrid :: RandomGen p => p -> Grid Char
makeRandomGrid gen =
    let (gen1, gen2) = split gen
        row = randomRs ('A', 'Z') gen1
    in row : makeRandomGrid gen2

zipOverGrid :: Grid a -> Grid b -> Grid (a, b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = map . map

cell2char :: Cell -> Char
cell2char (Cell _ c) = c
cell2char _ = '?'

coordsGrid :: Grid (Integer, Integer)
coordsGrid =
    let rows = map repeat [0..]
        cols = repeat [0..]
    in zipOverGrid rows cols

findWordInLine :: String -> [Cell] -> Maybe [Cell]
findWordInLine _ [] = Nothing
findWordInLine word line =
    let found = findWordInCellLinePrefix [] word line
    in case found of
        Nothing -> findWordInLine word (tail line)
        cs@(Just _) -> cs

findWordInCellLinePrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellLinePrefix acc (x: xs) (c:cs)  | x == cell2char c =
    findWordInCellLinePrefix (c : acc) xs cs
findWordInCellLinePrefix acc [] _ = Just $ reverse acc
findWordInCellLinePrefix _ _ _ = Nothing

getLines :: Grid Cell -> [[Cell]]
getLines grid =
    let horizontal = grid
        vertical = transpose grid
        diagonal1 = diagonalize horizontal
        diagonal2 = diagonalize $ gridReverse horizontal
        gridLines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
    in gridLines ++ gridReverse gridLines

gridReverse :: Grid Cell -> Grid Cell
gridReverse = map reverse

diagonalize :: Grid Cell -> Grid Cell
diagonalize = transpose . skew

skew :: Grid Cell -> Grid Cell
skew [] = []
skew (l:ls) = l : skew (map indent ls)
    where indent line = Indent : line