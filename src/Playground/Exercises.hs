module Playground.Exercises where

import Control.Lens
import Control.Lens.Tuple
import Control.Monad (msum)

import Data.Maybe (catMaybes)

-- | (x, y
type Move = (Int, Int, Player)

-- | Game is a sequence of moves, with X going first
type Game = [Move]

data Player
  = X
  | O
  deriving (Eq, Show)

type Row = (Player, Player, Player)

-- Need a working tic-tac-toe instance
type Board = [[Player]]

getWinner :: Board -> Maybe Player
getWinner =
  msum .
  concat .
  sequence [map wonSeq, map wonSeq . columns, leftDiag, rightDiag]
  where
    columns :: Board -> [[Player]]
    columns = sequence (map (map . (flip (!!))) [0, 1, 2])
    getIdx idx = pure . wonSeq . catMaybes . sequence (map (flip (^?)) idx)
    leftDiag = getIdx [ix 2 . ix 2, ix 1 . ix 1, ix 0 . ix 0]
    rightDiag = getIdx [ix 2 . ix 0, ix 1 . ix 1, ix 0 . ix 2]
    wonSeq [X, X, X] = Just X
    wonSeq [O, O, O] = Just O
    wonSeq _ = Nothing


board :: Board
board = [[O, X, O], [X, X, X], [O, O, X]]
