module Playground.Exercises where

import GHC.Err (errorWithoutStackTrace)

import Control.Arrow ((&&&))
import Control.Lens
import Control.Lens.Tuple
import Control.Monad (forever, msum)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Data.Foldable (traverse_)
import Data.List.Split (chunksOf)
import Data.List (elemIndices, maximumBy, intercalate)

import Data.Map (Map)
import Data.Maybe (isJust)
import qualified Data.Map as M

import Data.Maybe (catMaybes)
import Data.Ord (comparing)

import System.Random
import Text.Printf (printf)

-- | (x, y
type Move = (Int, Int, Player)

-- | Game is a sequence of moves, with X going first
type Game = [Move]

data Player
  = X
  | O
  | None
  deriving (Eq)
instance Show Player where
  show X = "X"
  show O = "O"
  show None = " "

-- | Board is a 2D representation of a Tic Tac Toe game
--
-- | 0, 1, 2 |
-- | 3, 4, 5 |
-- | 6, 7, 8 |
newtype Board = Board [Player]
instance Show Board where
  show = concat . map showRow . chunksOf 3 . toList
    where showRow :: [Player] -> String
          showRow = (++) "\n" . intercalate " | " . map show


toList :: Board -> [Player]
toList (Board b) = b

getWinner :: Board -> Maybe Player
getWinner =
  msum .
  concat .
  sequence [map wonSeq, map wonSeq . columns, leftDiag, rightDiag] . chunksOf 3 . toList
  where
    columns = sequence (map (map . (flip (!!))) [0, 1, 2])
    getIdx idx = pure . wonSeq . catMaybes . sequence (map (flip (^?)) idx)
    leftDiag = getIdx [ix 2 . ix 2, ix 1 . ix 1, ix 0 . ix 0]
    rightDiag = getIdx [ix 2 . ix 0, ix 1 . ix 1, ix 0 . ix 2]
    wonSeq [X, X, X] = Just X
    wonSeq [O, O, O] = Just O
    wonSeq _ = Nothing

hasWinner :: Board -> Bool
hasWinner = isJust . getWinner

-- Just some test board they're testing with
testBoard :: Board
testBoard = Board (concat [[None, X, None], [None, O, None], [None, X, None]])

-- You can use the same data to build the training model
-- Once you get data that you can use data to do it
-- Hashes the board the same way that players are hashed
hashBoard :: Board -> Int
hashBoard = foldl ((+) . (* 3)) 0 . map hashPlayer . toList
  where
    hashPlayer X = 1
    hashPlayer O = 2
    hashPlayer None = 0

type TicTacToe = Map Int Double
-- data TicTacToe = TicTacToe {
--       board :: Board
--     }
testState :: State Int Int
testState
 = do
  modify (+ 1)
  get

-- initialState :: Map Int Double
-- initialState = [()]
-- This is a recursive backtracking problem, so it shouldn't be too difficult.
-- Every time we find a new state, need to add it to the list though

newBoard :: Board
newBoard = Board (replicate 9 None)


-- | Initiate map with all states of the game
-- | All winning states are marked with 1, losing states are marked 0,
-- | and all other states are marked 0.5
initStates :: Board -> State TicTacToe ()
initStates board = do
  let h = hashBoard board
  m <- get

  unless (M.member h m) $ do
    let winningChances = case (getWinner board) of
                           Just X -> 1
                           Just O -> 0
                           _ -> 0.5
    put (M.insert h winningChances m)
    traverse_ initStates (getEdges board)

-- | Simple T-D learning method
updateRule :: StepSize -> Double -> Double -> Double
updateRule stepSize new old = old + stepSize * (new - old)

type StepSize = Double
stepSize :: StepSize
stepSize = 0.05

-- | Selects the next move based on what's has the highest value
bestMove :: Board -> State TicTacToe Board
bestMove board = do
  let hash = hashBoard board
  m <- get
  let best = maximumBy (comparing (flip find m . hashBoard)) (getEdges board)
  put $ M.insertWith (updateRule stepSize) hash 0 m
  return best
 where find = M.findWithDefault 0

-- | Makes the nth move of available edges
manualMove :: Board -> Int -> Board
manualMove board n = choice n (getEdges board)

playGame :: Board -> State TicTacToe ()
playGame board = do
  unless (hasWinner board) $ bestMove board >>= playGame

-- | Play Game using list of lazy seeds
playGameTwo :: [Int] -> Board -> State TicTacToe Board
playGameTwo (x:y:xs) board
  | hasWinner board = return board
  | x < 50 = return (manualMove board y) >>= playGameTwo xs
  | otherwise = bestMove board >>= playGameTwo xs

-- | Play X number of games
playNGames :: [Int] -> Int -> State TicTacToe ()
playNGames = flip replicateM_ (playGame newBoard)

-- | Checks the number of empty slots on the board to
-- find the next player
nextPlayer :: Board -> Player
nextPlayer (Board board) =
  if odd (count None board)
    then X
    else O
    -- Board & element row & element column .~ player

-- | Gets all the positions possible from board position
getEdges :: Board -> [Board]
getEdges b@(Board board) = do
  guard $ not (hasWinner b)
  let player = nextPlayer b
  s <- elemIndices None board
  return $ Board (board & ix s .~ player)

type Policy = Map Int Double

train :: Int -> Policy
train _ = M.empty

-- trainRandomly = do
--   -- gen <- newStdGen
--   initStates newBoard
--   loop
--   return ()
--  where loop = do
--    bestMove


getRandoms :: IO [Int]
getRandoms = take 10 . randomRs (0, 1000) <$> newStdGen

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

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

choice :: Int -> [a] -> a
choice _ [] = errorWithoutStackTrace "choice: empty list"
choice n xs = xs !! (mod n (length xs))

sumValues :: TicTacToe -> Double
sumValues = sum . map snd . M.toList
