module Playground.Exercises where

-- module tic tac toe
-- | (x, y
type Move = (Int, Int)

-- | Game is a sequence of moves, with X going first
type Game = [Move]

data Winner = X | O deriving (Eq, Show)

type Board = [[Int]]
-- hasWon :: Board -> Maybe Winner
-- hasWon board =


-- -- | Runs through the sequences of moves and determines the winner
-- runGame :: Game -> Winner
-- runGame g =
--     foldl
