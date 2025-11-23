import System.IO
import Data.Int
import GHC.Base (VecElem(Int64ElemRep))
import GHC.Arr
{-
run ghci othello-sequential.hs
then run main
-}


-- Game types
type Position = (Int, Int)

type Board = [[Int]]

type Player = Int

type PDisks = [Position]


data BoardState = BoardState {
    board       :: Board,
    curr_player :: Int,
    p1disks     :: PDisks,
    p2disks     :: PDisks
} deriving (Show)

rows :: Int
rows = 8

cols :: Int
cols = 8

gameBoard :: Board
gameBoard = [[0 | _ <- [1..cols]] | _ <- [1..rows]] :: [[Int]]

gameBoard ! 0 = [1,1,1,1,1,1,1,1]

updateBoardIndex :: Board -> Position -> Int -> Board
updateBoardIndex board (i,j) val = prevRows ++ [updatedRow] ++ afterRows where
    (prevRows, currRow : afterRows) = splitAt i board
    (prevElems, currElem : afterElems) = splitAt j currRow
    updatedRow = prevElems ++ [val] ++ afterElems

updateBoardIndexes :: Board -> [Position] -> [Int] -> Board
updateBoardIndexes board [] [] = board
updateBoardIndexes board (x:xs) (y:ys) = updateBoardIndexes newBoard xs ys where
    newBoard = updateBoardIndex board x y

{-
Given a board state, return the possible next moves for the player
-}
-- getPossibleMoves :: BoardState -> [Position]


{-
Given a Position and a BoardState, return whether the flanking condition is True/False
-}
-- flankingCheck :: Position -> BoardState -> Bool


{-
Given a Position and a BoardState, return whether the new Position already exists on the board. Can use as a helper method for getPossibleMoves
-}
-- checkExists :: Position -> BoardState -> Bool

{-
flipDisks, given BoardState, flip any disks for curr player
-}
-- flipDisks :: BoardState -> BoardState

{-
Given a BoardState, return heuristic score of board
-}
-- evaluateBoard :: BoardState -> Int

{-
checkWinner, return winner of board (1, 2), or 0 if tie
-}
-- checkWinner :: BoardState -> Int

{-
miniMax algo for deciding next moves
-}
-- miniMax

{-
gameLoop logic for alternating between players
-}
-- gameLoop :: BoardState -> IO ()

-- Printing for Board
printBoard :: Board -> IO ()
printBoard board = mapM_ (putStrLn . unwords . map show) board

main :: IO ()
main = do
    printBoard $ newBoard where
        newBoard = updateBoardIndexes gameBoard [(3,3),(4,4),(4,3),(3,4)] [1,1,2,2]









