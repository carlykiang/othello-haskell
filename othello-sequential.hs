import System.IO
import Data.Int
import GHC.Base (VecElem(Int64ElemRep))
import GHC.Arr

-- run ghci othello-sequential.hs
-- then main

-- Define game components
-- data Player = Player1 | Player2 deriving (Eq, Show) -- Might not need, can just keep track of ints 1 and 2 for pl1 pl2

-- type Player = 1 | 2 deriving (Eq, Show)

-- Board
type Position = (Int, Int)

type Board = [[Int]]


data BoardState = BoardState {
    board       :: Board,
    player       :: Int
} deriving (Show)

rows :: Int
rows = 8

cols :: Int
cols = 8

gameBoard :: Board
gameBoard = [[0 | _ <- [1..cols]] | _ <- [1..rows]] :: [[Int]]

gameBoard ! 0 = [1,1,1,1,1,1,1,1]

updateBoardIndex :: Board -> (Int, Int) -> Int -> Board
updateBoardIndex board (i,j) val = prevRows ++ [updatedRow] ++ afterRows where
    (prevRows, currRow : afterRows) = splitAt i board
    (prevElems, currElem : afterElems) = splitAt j currRow
    updatedRow = prevElems ++ [val] ++ afterElems


-- Printing for Board
printBoard :: Board -> IO ()
printBoard board = mapM_ (putStrLn . unwords . map show) board

main :: IO ()
main = do
    -- printBoard gameBoard
    printBoard $ updateBoardIndex gameBoard (0,0) 1








