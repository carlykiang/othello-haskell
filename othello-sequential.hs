import System.IO
import Data.Int
import GHC.Base (VecElem(Int64ElemRep))
import GHC.Arr
import System.Posix.Internals (puts)
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import Debug.Trace (trace)


{-
Commands to compile and run this program:
ghc -o othello othello-sequential.hs
./othello
-}


-- Game types
type Position = (Int, Int)

type Board = [[Int]]

type Player = Int -- 0 means not taken, 1 means taken by player 1, 2 means taken by player 2

type PDisks = [Position]


data BoardState = BoardState {
    board       :: Board,
    curr_player :: Int
    -- Temporarily commenting out PDisks to focus on core logic first
    -- p1disks     :: PDisks,
    -- p2disks     :: PDisks
} deriving (Show)

rows :: Int
rows = 8

cols :: Int
cols = 8

gameBoard :: Board
gameBoard = [[0 | _ <- [1..cols]] | _ <- [1..rows]] :: [[Int]]


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
adjacencyAndFlankingCheck bs (i,j) = [ d | d@(di,dj) <- directions, isAdjacentToOpp d && flankOpp d]
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

switchPlayer :: BoardState -> BoardState
switchPlayer bs = new_bs where
    new_bs = BoardState {
        board = board bs,
        curr_player = if curr_player bs == 1 then 2 else 1
    }

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
checkWinner, return winner of board (1, 2), or 0 if tie
-}

checkWinner :: BoardState -> Int
checkWinner bs
    | nomoves && p1Count > p2Count = 1
    | nomoves && p2Count > p1Count = 2
    | otherwise = 0
    where
        b = board bs
        nomoves = null (getPossibleMoves bs) && null (getPossibleMoves (switchPlayer bs))
        p1Count = sum [ length (filter (==1) row) | row <- b ]
        p2Count = sum [ length (filter (==2) row) | row <- b ]

{-
Given a BoardState, return heuristic score of board
-}
-- TODO
evaluateBoard :: BoardState -> Int 
evaluateBoard bs = 2*(cornerHeuristic bs) + (mobilityHeuristic bs)


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
miniMax algo for deciding next moves
Player 1: 1000
Player 2: -1000
depth starts at a user defined, until we reach 0
How to write minimax in haskell?
-}
-- TODO
minimax :: BoardState -> Maybe Position -> Int -> (Int, Maybe Position, BoardState)
minimax bs pos depth = 
    let b = board bs 
        best_move = pos
        cur_player = curr_player bs
    in case checkWinner bs of
        1 -> trace "Winner = Player 1 (score 100)" (100, best_move, bs)
        2 -> trace "Winner = Player 2 (score 100)" (-100, best_move, bs)
        0 -> 
            if depth == 0
                then trace "at depth = 0" (evaluateBoard bs, best_move, bs)
            else -- need to have logic for when there are no possible moves?
                let moves = getPossibleMoves bs 
                    scores = map (\pos -> trace ("Exploring move: " ++ show pos ++
                                               " at depth=" ++ show depth) 
                                               (minimax (updateTurn pos bs) (Just pos) (depth - 1))) moves
                in if null moves
                   then (0, Nothing, bs)
                   else 
                        if cur_player == 1 -- maximum
                        then maxIntBoard scores
                        else minIntBoard scores

maxIntBoard :: [(Int, Maybe Position, BoardState)] -> (Int, Maybe Position, BoardState)
maxIntBoard xs = maximumBy (comparing (\(a, _, _) -> a)) xs    


minIntBoard :: [(Int, Maybe Position, BoardState)] -> (Int, Maybe Position, BoardState)
minIntBoard xs = minimumBy (comparing (\(a, _, _) -> a)) xs   
  
computerTurn :: BoardState -> IO ()
computerTurn bs = do
    putStrLn "Computer turn"
    let (curr_score, best_move, new_bs) = minimax bs Nothing 1 -- minimax of depth 3
    let final_score = evaluateBoard new_bs
    let new_bs = case best_move of
            Nothing        ->  (switchPlayer bs) -- forfeit turn, give to player
            Just best_move ->  (updateTurn best_move bs)
    printBoard (board new_bs)
    putStrLn $ "Computer evaluated move with score: " ++ show final_score
    newGameLoop new_bs
    -- printBoard (board new_bs)

-- Just select the first move from the possible moves
playerTurn :: BoardState -> IO ()
playerTurn bs = do
    putStrLn "Player turn"
    let moves = getPossibleMoves bs 
    let new_bs = case null moves of
            True -> switchPlayer bs
            False  -> (updateTurn (head moves) bs)
    printBoard (board new_bs)
    newGameLoop new_bs

{-
Logic for player/computer back and forth game
-}
newGameLoop :: BoardState -> IO ()
newGameLoop bs = do
    putStrLn "New round"
    printBoard (board bs)
    case checkWinner bs of
        1 -> putStrLn "Player 1 won"
        2 -> putStrLn "Player 2 won"
        0 -> do
            if curr_player bs == 1
                then computerTurn bs
                else playerTurn bs
{-
gameLoop logic for alternating between players
We loop until both players have no possible moves, and then declare winner
This is a basic one to ensure that our helper functions work
miniMax an evaluateBoard are NOT implemented yet
TODO: Once those are implemented, we need to incorporate miniMax logic into gameLoop
-}
gameLoop :: BoardState -> IO ()
gameLoop bs = do
    let possibleMoves = getPossibleMoves bs
    if null possibleMoves
        then do
            let oppPlayer = if curr_player bs == 1 then 2 else 1
            let oppPossibleMoves = getPossibleMoves (BoardState { board = board bs, curr_player = oppPlayer })
            if null oppPossibleMoves
                then do
                    let winner = checkWinner bs
                    if winner == 0
                    then putStrLn "Game over! It's a tie!"
                    else putStrLn $ "Game over! Winner is Player " ++ show winner
                else do
                    putStrLn $ "Player " ++ show (curr_player bs) ++ " has no moves. Skipping turn."
                    gameLoop (BoardState { board = board bs, curr_player = oppPlayer })
        else do
            -- For simplicity, just pick the first possible move for now
            -- TODO: need to change this to incorporate miniMax logic later
            let (move:_) = possibleMoves
            putStrLn $ "Possible moves for Player " ++ show (curr_player bs) ++ ": " ++ show possibleMoves
            putStrLn $ "Player " ++ show (curr_player bs) ++ " places disc at " ++ show move
            putStrLn "BEFORE MOVE:"
            printBoard (board bs)
            let newGameState = updateTurn move bs
            putStrLn "AFTER MOVE:"
            printBoard (board newGameState)
            gameLoop newGameState

main :: IO ()
main = do
    -- gameLoop initializeBoard
    newGameLoop initializeBoard
    putStrLn "Thanks for playing!"
    










