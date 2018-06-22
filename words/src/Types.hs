module Types ( Cell (Cell, Indent)
           , Game (Game, gameGrid, gameWords)
           , Grid
           ) where

import qualified Data.Map as M

data Game = Game {
                    gameGrid :: Grid Cell,
                    gameWords :: M.Map String (Maybe [Cell])
                 } deriving Show

data Cell = Cell (Integer, Integer) Char
          | Indent
          deriving (Eq, Ord, Show)

type Grid a = [[a]]