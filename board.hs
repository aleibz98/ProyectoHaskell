import Data.List
import System.Random

-- NOTES TO IMPLEMENT 
    -- Shuold implement a module that represents a game state so a local search can be performed
    -- My IA should work with a tree of possible game states and where we can permorm a game theory local search
    -- game -> state -> turn, winner, isDraw, Board -> [[Maybe Int]]
    -- globals -> difficultity, numRows, numCols




module State (
    -- Atributes
    turn
    winner
    isDraw
    _board

    -- Methods
    newBoard :: Board

    checkVertical :: (Row, Column) -> Board
    checkHorizontal :: 
    checkDiagDown ::
    checkDiagUp ::
    checkAll ::
) where
    newBoard (i,j) = Con (replicate j []) (i,j)
    checkVertical :: 
    checkHorizontal :: 
    checkDiagDown ::
    checkDiagUp ::
    checkAll ::

module Board (

) where

---------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------  

-- Type to codify a position by its coords
type Row = Int
type Column = Int
type Position = (Row, Column)

winner = 0
draw = false
diff = 0
turn = false            -- True -> USER || False -> IA

data Board = Con [[Player]] (Row, Column) deriving (Eq, Show)


-- We will be using a board formed by lists of players, where every element is equivalent to a token of that player
    -- We will be using the definition Maybe player
-- Creation of empty board
newBoard :: (Row, Column) -> Board
newBoard (i, j) = Con (replicate j []) (i,j)

-- CHECKS if a movement is winner
checkVertical :: Board -> Position -> Player -> Bool
checkVertical b pos player =

checkHorizontal :: Board -> Position -> Player -> Bool
checkHorizontal b pos player = 

checkDiagUp :: Board -> Position -> Player -> Bool
checkDiagUp b pos player = 

checkDiagDown :: Board -> Position -> Player -> Bool
checkDiagDown b pos player = 

checkAll :: Board -> Position -> Player -> Bool
checkAll b pos player = do
    return (checkVertical b pos player || checkDiagDown b pos player || checkDiagUp b pos player || checkHorizontal b pos player)



-- Make Movement - returns true if movement was succesfully executed
    -- Places a token for the player in the chosen column's minimum position
makeMove :: Board -> Player -> Column -> Bool
makeMove b p c = 
    -- Need to check if movement is valid
    -- Need to checkAll


randInt :: Int -> Int -> IO Int
randInt low high = do
    random <- randomIO :: IO Int
    let result = low + random 'mod' (high - low + 1)
    return result


-- IA 
ia :: Board -> Int -> Int
-- random - outputs a number randomly between 0 and 6 (both included)
ia _ 1 = randInt 0 7

--greedy - only cares about winning, doesn't care about not losing. Isn't aware of the opponent
ia board 2 = do

--smart - is aware of the opponent and cares about winning and not losing
ia board 3 = do

-- PLAY
-- Main method for the game
play :: Board -> IO()
play board
    | winner == 1 = putStrLn "YOU WIN"
    | winner == 2 = putStrLn "YOU LOSE"
    | draw == true = "DRAW"
    | otherwise = do
        x <- readMove turn
        if (isLegalMove x) then
            let board1 = executeMove board x turn
            turn = !turn
            --Show board after executing the movement
            play board1
        else play board


-- Executes the move for the player
executeMove :: Board -> Column -> Bool -> Board
executeMove board col player = do
    -- Magic happens
    return board

-- Checks wether or not a movement is legal
    -- That means checking if 0 <= Column <= 7 and that the column is not full
isLegalMove :: Board -> Column -> Bool
isLegalMove b c = 

-- Reads an integer from stdIn equivalent to the movement to be made by the player
readMove :: Bool -> IO Int
-- CALL IA
readMove false = do
        return (ia diff)
-- Users movement
readMove true = do                      
        putStr "Enter your move: "
        x <- getLine
        return (read x)

-- Choose the difficultity of the IA
chooseIA :: IO Int 
chooseIA = do
    putStrLn "Choose the difficultity:"
    putStrLn "1. Random"
    putStrLn "2. Greedy"
    putStrLn "3. Smart"
    x <- getLine
    return (read x)


main :: IO()
main = do
    diff = chooseIA
    let t = rand 0 1
        if(t == 0) then turn = false
        else turn = true
    play (newBoard (6,7))