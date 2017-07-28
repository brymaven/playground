module Playground.Exercises where

import Control.Arrow ((&&&))
import Control.Lens
import Control.Lens.Tuple
import Control.Monad (forever, msum)
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe (catMaybes)

-- | (x, y
type Move = (Int, Int, Player)

-- | Game is a sequence of moves, with X going first
type Game = [Move]

data Player
  = X
  | O
  | None
  deriving (Eq, Show)

-- | Board is a 2D representation of a Tic Tac Toe game
--
-- | 0, 1, 2 |
-- | 3, 4, 5 |
-- | 6, 7, 8 |
type Board = [Player]
getWinner :: [[Player]] -> Maybe Player
getWinner =
  msum .
  concat . sequence [map wonSeq, map wonSeq . columns, leftDiag, rightDiag]
  where
    columns = sequence (map (map . (flip (!!))) [0, 1, 2])
    getIdx idx = pure . wonSeq . catMaybes . sequence (map (flip (^?)) idx)
    leftDiag = getIdx [ix 2 . ix 2, ix 1 . ix 1, ix 0 . ix 0]
    rightDiag = getIdx [ix 2 . ix 0, ix 1 . ix 1, ix 0 . ix 2]
    wonSeq [X, X, X] = Just X
    wonSeq [O, O, O] = Just O
    wonSeq _ = Nothing

-- Just some test board they're testing with
testBoard :: Board
testBoard = concat [[None, O, None], [None, None, None], [None, O, None]]

-- You can use the same data to build the training model
-- Once you get data that you can use data to do it
-- Hashes the board the same way that players are hashed
hashBoard :: Board -> Int
hashBoard = foldl ((+) . (* 3)) 0 . map hashPlayer
  where
    hashPlayer X = 1
    hashPlayer O = 2
    hashPlayer None = 0

-- foldl (\acc i -> (3 * acc) + i) 0 [1,2,3,6]
-- data TicTacToe = TicTacToe {
--       board :: Board
--     }
testState :: State Int Int
testState
  -- put 3
 = do
  modify (+ 1)
  get

-- initialState :: Map Int Double
-- initialState = [()]

-- This is a recursive backtracking problem, so it shouldn't be too difficult.
-- Every time we find a new state, need to add it to the list though

-- allStates :: []
-- allStates
-- No real reason to have 2D array other than convenience right?


-- nextState :: Board -> Move -> Board
-- nextState board (row, column, player) = Board & element row & element column .~ player



-- Set up a table of numbers, one for each possible state of the game.
-- Estimate is the state's value and the whole table is the learned value function.z
-- This really will just be a State IO Monad
-- Occassionally, exporatory ratio, select randomly
-- Make the numbers more accurate during the game.
-- "back up" the value of the state after each greedy move
-- The current value of the earlier state is adjusted to be closer to the value of the later state.
-- For exploratory moves, don't need to backup the state.
-- V(s) <- V(s) + alpha [ V(s') - V(s) ]
-- data Reinforcement = Reinforcement {
--       environment :: Int
--     , stepSize :: Double        -- | Learning rate
--     }
-- nextMove :: Reinforcement ()
-- nextMove = j
type Policy = Map Int Double

train :: Int -> Policy
train _ = M.empty

parseInput :: Int -> IO ()
parseInput i
  | 0 <= i && i < 9 =
    putStrLn $ "Handling input at row at " ++ show (getPosition i)
  | otherwise = putStrLn "Invalid selection. Choose between 0 and 8"
  where
    getPosition = ((`quot` 3) &&& (`mod` 3))

-- | Train and then play TicTacToe
play :: IO ()
play = loop
  where
    loop = do
      s <- getLine
      if s == "q"
        then return ()
        else ((parseInput . read) s) >> loop
