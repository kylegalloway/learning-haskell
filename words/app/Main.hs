module Main where

import Lib (completed, fillInBlanks, formatGame, Game, grid, languages, makeGame, playGame)
import System.IO
import System.Random

main :: IO ()
main = do
    gen <- newStdGen
    let filledInGrid = fillInBlanks gen grid
        game = makeGame filledInGrid languages
    hSetBuffering stdout NoBuffering
    playTurn game

playTurn :: Game -> IO ()
playTurn game = do
    putStrLn . formatGame $ game
    putStr "Please enter a word> "
    word <- getLine
    let newGame = playGame game word
    if completed newGame then
        putStrLn "Congratulations!"
    else
        playTurn newGame
