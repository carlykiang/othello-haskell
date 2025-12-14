import System.Random (StdGen, newStdGen, randomR)
import Data.List (maximumBy)
import Data.Ord (comparing)

{-
Commands to compile and run this program:
stack install random
TODO: set it up as stack project with proper .yaml files in the long run to make compilations easier
For now, run the following command in terminal to compile and run:
stack ghc  --package random  -- -Wall -O2 -o othello-mcts-seq othello-mcts-seq.hs
./othello-mcts-seq
-}

-- Game types
type Position = (Int, Int)

type Board = [[Int]] -- 0: empty, 1: player 1's disk, 2: player 2's disk

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
Commented out because noPrintGameLoop will not print boards
-}
-- printBoard :: Board -> IO ()
-- printBoard board = mapM_ (putStrLn . unwords . map show) board


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
adjacencyAndFlankingCheck bs (i,j) = [ d | d <- directions, isAdjacentToOpp d && flankOpp d]
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

-- MCTS related functions start here
-- Tutorial on MCTS, where the following code snippets are based off: https://www.youtube.com/watch?v=UXW2yZndl7U&t=1s
{-
MCTS Node data structure
-}
data MCTSNode = MCTSNode {
    state       :: BoardState,
    children    :: [MCTSNode],
    n_i         :: Int,
    t           :: Double,
    m           :: Maybe Position -- The move that led to this state; Nothing for root node so we use Maybe
} deriving (Show)

{-
Calculate UCT value for a given MCTSNode
-}
uctValue :: MCTSNode -> Int -> Double
uctValue node parentN =
    if n_i node == 0 then 1/0 -- infinity
    else (t node / fromIntegral (n_i node)) + (sqrt 2) * sqrt (log (fromIntegral parentN) / fromIntegral (n_i node))

{-
Selection: select the best child to perform simulation based on UCT values
Also keeps track of path from root to selected node for backpropagation later
Boolean for if the leaf has been visited before return True (need to expand)
    if leaf has not been visited before, return false, can directly simulate/rollout
-}
selection :: MCTSNode -> [MCTSNode] -> (MCTSNode, [MCTSNode], Bool)
selection node path =
    if null (children node)
        then if n_i node == 0
            then (node, path, True)
            else (node, path, False)
        else selection bestChild (path ++ [node])
    where
        bestChild = maximumBy (comparing (\child -> uctValue child (n_i node))) (children node)



{-
Expansion: expand the selected node by adding a new child node for one of its possible moves pos
return the expanded selected node as well as the new child node
-}
expansion :: MCTSNode -> Position -> (MCTSNode, Maybe MCTSNode)
expansion node pos =
    if any (\c -> m c == Just pos) (children node)
        then (node, Nothing) -- Child already exists, do not add duplicate
        else (node { children = newNode : children node }, Just newNode)
    where
        bsNew = updateTurn pos (state node)
        newNode = MCTSNode {
            state = bsNew,
            children = [],
            n_i = 0,
            t = 0.0,
            m = Just pos
        }

{-
Simulation: simulate a random playout from the given node's state until terminal state is reached
t will be set to be heuristic score of the BoardState reached at terminal state, given by evaluateBoard
-}
simulation :: MCTSNode -> IO Double
simulation node = do
    gen <- newStdGen
    return $ simulateFromState (state node) gen

simulateFromState :: BoardState -> StdGen -> Double
simulateFromState bs g =
    let moves = getPossibleMoves bs
    in if null moves
        then evaluateBoardMTCS bs
        else
            let (randomIndex, g') = randomR (0, length moves - 1) g
                move = moves !! randomIndex
                newBs = updateTurn move bs
            in simulateFromState newBs g'

evaluateBoardMTCS :: BoardState -> Double
evaluateBoardMTCS bs = fromIntegral (p2Count - p1Count)
    where
        p1Count = countDisc bs 1
        p2Count = countDisc bs 2

{-
Backpropagation: propagate n_i and t values along the path from selected node back to root
Also update children field of the node along the way
[MCTSNode] is of the path from leaf to root, so we use the previous node to update the children field of the current node
MCTSNode: previous node updated (the node we will use to update children field of current node in the path)
Double: reward obtained from simulation
MCTSNode: return the last node updated (AKA root node updated)
-}
backpropagation :: [MCTSNode] -> MCTSNode -> Double -> MCTSNode
backpropagation [] updatedNode _ = updatedNode
backpropagation (parent:rest) updatedChild reward =
    let updatedChild' = updatedChild { n_i = n_i updatedChild + 1, t = t updatedChild + reward }
        updatedParent = parent {
            children = map (\c -> if m c == m updatedChild' then updatedChild' else c) (children parent),
            n_i = n_i parent + 1,
            t = t parent + reward
        }
    in backpropagation rest updatedParent reward

{-
Run one iteration of MCTS: 
1. selection
2. expansion
3. simulation
4. backpropagation
-}
runMCTSIteration :: MCTSNode -> IO MCTSNode
runMCTSIteration root = do
    -- 1. selection, select the best leaf, starting from the root
    let (selectedNode, path, _) = selection root []
    case m selectedNode of
        Nothing -> return root -- Root it selected, try expanding from the root
        Just _ -> do  -- See if we can try expansion
            let possibleMoves = getPossibleMoves (state selectedNode)
            if null possibleMoves
            then do
                -- Terminal node: still run simulation and backpropagation to update n_i and t values
                reward <- simulation selectedNode
                let updatedRoot = backpropagation (reverse path) selectedNode reward
                return updatedRoot
            else do 
                gen <- newStdGen
                let (randomIndex, _) = randomR (0, length possibleMoves - 1) gen
                let move = possibleMoves !! randomIndex
                let (expandedNode, potentialChild) = expansion selectedNode move -- 2. Expansion
                case potentialChild of
                    Nothing             -> return root -- we could not successfull expand
                    Just pc -> do
                        reward <- simulation pc -- 3. Simulation
                        -- expandedNode is the parent of the potentialChild
                        let updatedRoot = backpropagation (expandedNode : reverse path) pc reward
                        return updatedRoot

{-
Run MCTS for a given number of iterations starting from the given root node
Run MCTS for a given number of iterations starting from the given root node
MCTSNode: root of tree
[Maybe Position]: moves to get the child
MCTSNode: child we are adding
return new MCTSNode root where the child has been added
-}
runMCTS :: MCTSNode -> Int -> IO MCTSNode
runMCTS root 0 = do
    return root
runMCTS root n = do
    newRoot <- runMCTSIteration root
    runMCTS newRoot (n - 1)

-- MCTS related functions end here


{-
gameLoop logic for alternating between players
-- To illustrate that the MCTS is working, we make player 2 use miniMax to pick its moves
-- For now, player 1 will just pick a random vailable move
-- We should be able to see player 2 winning more often than player 1
-}
-- gameLoop :: BoardState -> IO ()
-- gameLoop bs = do
--     let possibleMoves = getPossibleMoves bs
--     if null possibleMoves
--         then do
--             let oppPlayer = if curr_player bs == 1 then 2 else 1
--                 oppPossibleMoves = getPossibleMoves (BoardState { board = board bs, curr_player = oppPlayer })
--             if null oppPossibleMoves
--                 then do
--                     let winner = checkWinner bs
--                     if winner == 0
--                     then putStrLn "Game over! It's a tie!"
--                     else do
--                         putStrLn $ "Game over! Winner is Player " ++ show winner
--                         -- putStrLn $ "Final Board:"
--                         -- printBoard (board bs)
--                         putStrLn $ "Player 1 discs: " ++ show (countDisc bs 1)
--                         putStrLn $ "Player 2 discs: " ++ show (countDisc bs 2)
--                 else do
--                     putStrLn $ "Player " ++ show (curr_player bs) ++ " has no moves. Skipping turn."
--                     gameLoop (BoardState { board = board bs, curr_player = oppPlayer })
--     else do
--         -- putStrLn $ "Possible moves for Player " ++ show (curr_player bs) ++ ": " ++ show possibleMoves
--         move <- if curr_player bs == 1
--                     then do
--                         -- Pick a random move for player 1
--                         gen <- newStdGen
--                         let (randomIndex, _) = randomR (0, length possibleMoves - 1) gen
--                         let move = possibleMoves !! randomIndex
--                         -- putStrLn $ "Player 1 (Random) chooses move " ++ show move
--                         return move
--                     else do
--                         -- putStrLn $ "Player 2 (MCTS) chooses move "
--                         let rootNode = MCTSNode {
--                             state = bs,
--                             children = [ c | mv <- possibleMoves,
--                                             let bsNew = updateTurn mv bs,
--                                             let c = MCTSNode {
--                                                 state = bsNew,
--                                                 children = [],
--                                                 n_i = 0,
--                                                 t = 0.0,
--                                                 m = Just mv
--                                             }
--                                             ],
--                             n_i = 0,
--                             t = 0.0,
--                             m = Nothing
--                         }
--                         mctsRoot <- runMCTS rootNode 1000 -- Run MCTS for 1000 iterations
--                         -- Print statements to make sure the MTCS actually works and results are propagated back to root
--                         putStrLn $ "Possible moves for Player 2 " ++ show (curr_player bs) ++ ": " ++ show possibleMoves
--                         putStrLn $ "MCTS Root after iterations: " ++ show mctsRoot
--                         let bestChild = maximumBy (comparing (\child -> uctValue child (n_i mctsRoot))) (children mctsRoot)
--                         let move = case m bestChild of
--                                         Just pos -> pos
--                                         Nothing -> error "No move found. Something went wrong with MCTS implementation."
--                         -- putStrLn $ "Player 2 (MCTS) chooses move " ++ show move
--                         return move
--         -- putStrLn $ "Player " ++ show (curr_player bs) ++ " places disc at " ++ show move
--         -- putStrLn "BEFORE MOVE:"
--         -- printBoard (board bs)
--         let newGameState = updateTurn move bs
--         -- putStrLn "AFTER MOVE:"
--         -- printBoard (board newGameState)
--         gameLoop newGameState

{-
noPrintGameLoop logic for alternating between players without printing boards
-}
noPrintGameLoop :: BoardState -> IO ()
noPrintGameLoop bs = do
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
                        putStrLn $ "Player 1 discs: " ++ show (countDisc bs 1)
                        putStrLn $ "Player 2 discs: " ++ show (countDisc bs 2)
                else do
                    putStrLn $ "Player " ++ show (curr_player bs) ++ " has no moves. Skipping turn."
                    noPrintGameLoop (BoardState { board = board bs, curr_player = oppPlayer })
    else do
        move <- if curr_player bs == 1
                    then do
                        -- Pick a random move for player 1
                        gen <- newStdGen
                        let (randomIndex, _) = randomR (0, length possibleMoves - 1) gen
                        let move = possibleMoves !! randomIndex
                        return move
                    else do
                        let rootNode = MCTSNode {
                            state = bs,
                            children = [ c | mv <- possibleMoves,
                                            let bsNew = updateTurn mv bs,
                                            let c = MCTSNode {
                                                state = bsNew,
                                                children = [],
                                                n_i = 0,
                                                t = 0.0,
                                                m = Just mv
                                            }
                                            ],
                            n_i = 0,
                            t = 0.0,
                            m = Nothing
                        }
                        mctsRoot <- runMCTS rootNode 10000 -- Run MCTS for 10000 iterations
                        let bestChild = maximumBy (comparing (\child -> uctValue child (n_i mctsRoot))) (children mctsRoot)
                        let move = case m bestChild of
                                        Just pos -> pos
                                        Nothing -> error "No move found. Something went wrong with MCTS implementation."
                        return move
        let newGameState = updateTurn move bs
        noPrintGameLoop newGameState

main :: IO ()
main = do
    -- gameLoop initializeBoard
    noPrintGameLoop initializeBoard
    putStrLn "Thanks for playing!"
    -- Simple testing for heuristics, can delete later

