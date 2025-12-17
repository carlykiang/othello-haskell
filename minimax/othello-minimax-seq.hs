import System.IO()
import Data.Int()
import GHC.Base()

import System.Posix.Internals()
import Data.List (maximumBy)
import Data.Ord()
import Debug.Trace()
import System.Random(newStdGen, randomR) 

{-
This file contains logic for sequential minimax without alpha beta pruning
The main function runs a playthrough of an Othello game, and prints
information for each turn. (Board state, available moves, selected move)

Commands to compile and run this program:
stack install random
stack ghc  --package random  -- -Wall -O2 -o othello othello-minimax-seq.hs
./othello
-}


-- Game types
type Position = (Int, Int)

type Board = [[Int]]

data BoardState = BoardState {
    board       :: Board,
    curr_player :: Int
} deriving (Show)

rows :: Int
rows = 8

cols :: Int
cols = 8

gameBoard :: Board
gameBoard = [[0 | _ <- [1..cols]] | _ <- [1..rows]] :: [[Int]]


updateBoardIndex :: Board -> Position -> Int -> Board
updateBoardIndex board (i,j) val = 
    case splitAt i board of
        (prevRows, currRow : afterRows) ->
            case splitAt j currRow of
                (prevElems, _ : afterElems) -> 
                    prevRows ++ [prevElems ++ [val] ++ afterElems] ++ afterRows
                (_, _) -> board
        (_, _) -> board


updateBoardIndexes :: Board -> [Position] -> [Int] -> Board
updateBoardIndexes board (x:xs) (y:ys) = updateBoardIndexes newBoard xs ys where
    newBoard = updateBoardIndex board x y
updateBoardIndexes board _ _ = board

{-
Initialize the gameboard to start configuration and set current player to 1
-}
initializeBoard :: BoardState
initializeBoard = BoardState {
    board = updateBoardIndexes gameBoard [(3,3),(4,4),(4,3),(3,4)] [1,1,2,2],
    curr_player = 1
}

{-
Printing for Board
-}
printBoard :: Board -> IO ()
printBoard board = mapM_ (putStrLn . unwords . map show) board


{-
Return the number at index (i,j) of the board
-}
getBoardVal :: Board -> Position -> Int
getBoardVal board (i,j) = (board !! i) !! j

{-
Defined 8-way directions for checking adjacency and flanking rules
-}
directions :: [Position]
directions = [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)]

{-
Check if a given position is within bounds of the board
-}
inBounds :: Position -> Bool
inBounds (x,y) = x >= 0 && x < rows && y >= 0 && y < cols

{-
Given a board state, return the possible next moves for the player
-}
getPossibleMoves :: BoardState -> [Position]
getPossibleMoves bs = [ (i,j) | i <- [0..rows-1],
                                j <- [0..cols-1],
                                getBoardVal (board bs) (i,j) == 0, -- spot is not taken yet
                                not (null (adjacencyAndFlankingCheck bs (i,j))) -- adjacency and flanking rules are both met
                                ]

{-
Check if the adjacency and flanking conditions are both met for a given position
Return a list of directions where both conditions are met
If the list is non-empty, then the position is a valid move that satisfy both adjancy and flankinng rules
-}
adjacencyAndFlankingCheck :: BoardState -> Position -> [Position]
adjacencyAndFlankingCheck bs (i,j) = [ d | d@(_,_) <- directions, isAdjacentToOpp d && flankOpp d]
    where
        b = board bs
        curPlayer = curr_player bs
        oppPlayer = if curPlayer == 1 then 2 else 1

        isAdjacentToOpp (di,dj) = inBounds (i+di,j+dj) && getBoardVal b (i+di,j+dj) == oppPlayer

        {- 
        Because of short-circuiting nature of &&, flankOpp will only be called if isAdjacentToOpp is True
        for the given direction (di,dj). As such, inside flankOpp, we can simplify checking flanking condition
        to be scanning towards direction (di,dj) and returning True if we find a disc of current player before hitting
        an empty spot or going out of bounds. Hitting a disc of current player means flanking condition is met in the
        (di,dj) direction because opponent's disc(s) is sandwiched between the new position (i,j) current player is
        going to take and current player's existing disc on board.
        -}
        flankOpp (di,dj) = scan (i + di + di) (j + dj + dj)
            where
                scan x y =
                    if not (inBounds (x,y)) then False
                    else case getBoardVal b (x,y) of
                        val | val == curPlayer -> True
                        val | val == oppPlayer -> scan (x + di) (y + dj)
                        _ -> False

{-
Given the position of the to-be-placed disc of current player and BoardState, update the discs on board and change curr_player to opponent player
to return an up-to-date BoardState to keep the game going
-}
updateTurn :: Position -> BoardState -> BoardState
-- Place a disc at `pos` for `curPlayer` and flip any sandwiched opponent disks
updateTurn (i,j) bs = BoardState {
    board = updatedBoard,
    curr_player = oppPlayer
}
    where
        b = board bs
        curPlayer = curr_player bs
        oppPlayer = if curPlayer == 1 then 2 else 1
        -- Figure out which directions are valid for flipping
        flippableDirections = adjacencyAndFlankingCheck bs (i,j)
        -- Helper function to get positions of all opponent discs to be flipped in one direction (di,dj)
        flipInDirection (di,dj) = scan (i + di) (j + dj) []
            where
                scan x y acc =
                    if not (inBounds (x,y)) then []
                    else case getBoardVal b (x,y) of
                        val | val == oppPlayer -> scan (x + di) (y + dj) ((x,y):acc)
                        val | val == curPlayer -> acc
                        _ -> []
        allPositionsToFlip = concatMap flipInDirection flippableDirections
        -- Replace index (i,j) and allPositionsToFlip with curPlayer
        updatedBoard = updateBoardIndexes b ((i,j):allPositionsToFlip) (replicate (1 + length allPositionsToFlip) curPlayer)

{-
Count number of discs of given player on board
-}
countDisc :: BoardState -> Int -> Int
countDisc bs player = sum [ length (filter (==player) row) | row <- board bs ]

{-
checkWinner, return winner of board (1, 2), or 0 if tie
-}

checkWinner :: BoardState -> Int
checkWinner bs
    | p1Count > p2Count = 1
    | p2Count > p1Count = 2
    | otherwise = 0
    where
        p1Count = countDisc bs 1
        p2Count = countDisc bs 2

{-
Given a BoardState, return heuristic score of board
-}
evaluateBoard :: BoardState -> Int
evaluateBoard bs = 2*(cornerHeuristic bs) + (mobilityHeuristic bs) + (discCountHeuristic bs)


{-
Given a BoardState, return higher scores if current player has corners
-}

cornerHeuristic :: BoardState -> Int
cornerHeuristic bs = c1 + c2 + c3 + c4 where
    b = board bs
    curPlayer = curr_player bs
    c1 = if getBoardVal b (0,0) == curPlayer then 2 else 0
    c2 = if getBoardVal b (0,7) == curPlayer then 2 else 0
    c3 = if getBoardVal b (7,0) == curPlayer then 2 else 0
    c4 = if getBoardVal b (7,7) == curPlayer then 2 else 0

{-
Given a BoardState, return higher scores if current player has more moves
-}
mobilityHeuristic :: BoardState -> Int
mobilityHeuristic bs = length pos where
    pos = getPossibleMoves bs

{-
Given a Board state, return higher score if current player has more discs on board by taking this move
-}
discCountHeuristic :: BoardState -> Int
discCountHeuristic bs = (playerCount - oppCount) `div` 3
    where
        curPlayer = curr_player bs
        oppPlayer = if curPlayer == 1 then 2 else 1
        playerCount = countDisc bs curPlayer
        oppCount = countDisc bs oppPlayer


{-
miniMax algo
isMaxizingPlayer: True if current player is maximizing player, False if minimizing player
-}
miniMax :: BoardState -> Int -> Bool -> Int
miniMax bs remainingDepth isMaximizingPlayer
    | remainingDepth == 0 || null moves = evaluateBoard bs
    | isMaximizingPlayer = maximum [miniMax (updateTurn move bs) (remainingDepth-1) False | move <- moves]
    | otherwise          = minimum [miniMax (updateTurn move bs) (remainingDepth-1) True | move <- moves]
    where
        moves = getPossibleMoves bs





{-
gameLoop logic for alternating between players
-}
gameLoop :: BoardState -> IO ()
gameLoop bs = do
    let possibleMoves = getPossibleMoves bs
    if null possibleMoves
        then do
            let oppPlayer = if curr_player bs == 1 then 2 else 1
                oppPossibleMoves = getPossibleMoves (BoardState { board = board bs, curr_player = oppPlayer })
            if null oppPossibleMoves
                then do
                    let winner = checkWinner bs
                    if winner == 0
                    then putStrLn "Game over! It's a tie!"
                    else do
                        putStrLn $ "Game over! Winner is Player " ++ show winner
                        putStrLn $ "Final Board:"
                        printBoard (board bs)
                        putStrLn $ "Player 1 discs: " ++ show (countDisc bs 1)
                        putStrLn $ "Player 2 discs: " ++ show (countDisc bs 2)
                else do
                    putStrLn $ "Player " ++ show (curr_player bs) ++ " has no moves. Skipping turn."
                    gameLoop (BoardState { board = board bs, curr_player = oppPlayer })
    else do
        putStrLn $ "Possible moves for Player " ++ show (curr_player bs) ++ ": " ++ show possibleMoves
        move <- if curr_player bs == 1
                    then do
                        -- Pick a random move for player 1
                        gen <- newStdGen
                        let (randomIndex, _) = randomR (0, length possibleMoves - 1) gen
                        let move = possibleMoves !! randomIndex
                        putStrLn $ "Player 1 (Random) chooses move " ++ show move
                        return move
                    else do
                        putStrLn $ "Player 2 (MiniMax) chooses move "
                        let possibleMovesWithScores = [ (m, miniMax (updateTurn m bs) 3 True) | m <- possibleMoves ]
                        putStrLn "Player 2 possible moves and scores:"
                        mapM_ (\(m,score) -> putStrLn $ "Move: " ++ show m ++ ", Score: " ++ show score) possibleMovesWithScores
                        let move = fst $ maximumBy (\(_,score1) (_,score2) -> compare score1 score2) possibleMovesWithScores

                        putStrLn $ "Player 2 (MiniMax) chooses move " ++ show move
                        return move
        putStrLn $ "Player " ++ show (curr_player bs) ++ " places disc at " ++ show move
        putStrLn "BEFORE MOVE:"
        printBoard (board bs)
        let newGameState = updateTurn move bs
        putStrLn "AFTER MOVE:"
        printBoard (board newGameState)
        gameLoop newGameState

main :: IO ()
main = do
    gameLoop initializeBoard
    putStrLn "Thanks for playing!"
