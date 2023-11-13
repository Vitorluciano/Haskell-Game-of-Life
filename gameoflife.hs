import System.Random
import Control.Concurrent (threadDelay)
import System.Info (os)
import System.Process (system)
import System.IO

--vivo: \128104
--zumbi: \129503

--global variables
alive = 'A'
dead = ' '
zombie = 'Z'

--EXTRA: FUNCTIONS TO CLEAR THE TERMINAL AFTER UPDATING THE GRID

-- Get the clear command based on the operating system
getClearCommand :: IO String
getClearCommand = return $ if isWindowsOS then "cls" else "clear"

-- Check if the operating system is Windows
isWindowsOS :: Bool
isWindowsOS = os == "mingw32"

------------------------------------------------------------------

--fill row with random values from a predefined list
fillRow :: Int -> StdGen -> String
fillRow numItems gen = [values !! i | i <- randomIndexes]
    where values = [alive, dead, zombie] 
          randomIndexes = (take numItems $ randomRs (0, 2) gen) 

--fill a matrix with random values from a predefined list
fillMatrix :: Int -> Int -> StdGen -> [String]
fillMatrix numRows numColumns gen = [ fillRow numColumns (generators !! j) | j <- [0 .. (numRows - 1)] ]
    where generators = take numRows $ iterate (snd . next) gen -- provavelmente o next que está causando problema no random

--turn a matrix of strings into a single String with a breakline between rows
toLine :: [String] -> String
toLine matrix = concat [ matrix !! i ++ "\n" | i <- [0 .. (length matrix - 1)]]

--print matrix
printString :: String -> IO ()
printString str = putStr str

--get all the neighbors of a cell
--i and j are the line and column of a cell
getNeighbors :: Int -> Int -> [String] -> String                                                                                                                                          --used equivalence p -> q then ¬p v q
getNeighbors i j matrix = [matrix !! iNgbr !! jNgbr | iNgbr <- [(i - 1) .. (i + 1)], jNgbr <- [(j - 1) .. (j + 1)], iNgbr >= 0 && jNgbr >= 0 && iNgbr < totalRows && jNgbr < totalCols && (iNgbr /= i || jNgbr /= j)]
    where totalRows = length matrix
          totalCols = length (matrix !! 0)

countOcurrencies :: String -> Char -> Int
countOcurrencies list value = sum [if x == value then 1 else 0 | x <- list]

--update cell according to the game rules
updateCell :: Char -> String -> Char
updateCell cell neighbors
    | cell == alive = updateAlive neighbors
    | cell == dead = updateDead neighbors
    | cell == zombie = updateZombie neighbors

updateDead :: String -> Char
updateDead neighbors
    | aliveNeighbors == 3 = alive
    | otherwise = dead
    where aliveNeighbors = countOcurrencies neighbors alive 

updateZombie :: String -> Char
updateZombie neighbors
    | aliveNeighbors == 0 = dead
    | otherwise = zombie
    where aliveNeighbors = countOcurrencies neighbors alive

updateAlive :: String -> Char
updateAlive neighbors
    | zombieNeighbors >= 1 = zombie
    | aliveNeighbors < 2 = dead
    | aliveNeighbors > 3 = dead
    | otherwise = alive
    where zombieNeighbors = countOcurrencies neighbors zombie
          aliveNeighbors = countOcurrencies neighbors alive

--update matrix according to the game rules
updateMatrix :: [String] -> [String]
updateMatrix currentMatrix = [ [updateCell (currentMatrix !! i !! j) (getNeighbors i j currentMatrix) | j <- columnsIndexes] | i <- rowsIndexes ]
    where rowsIndexes = [0 .. (length currentMatrix - 1)]
          columnsIndexes = [0 .. (length (currentMatrix !! 0) - 1)]

iterate' :: [String] -> Int -> [String]
iterate' 0 matrix = 
iterate' numIterations matrix = 

main = do
    clearCommand <- getClearCommand
    _ <- systemClearCommand

    putStrLn "Enter the number of rows of the matrix: "
    numRows <- getLine
    let rows = (read numRows :: Int)
    
    putStrLn "Enter the number of columns of the matrix: "
    numColumns <- getLine
    let cols = (read numColumns :: Int)

    putStrLn "Enter the number of iterations: "
    numIterations <- getLine
    let iterations = (read numIterations :: Int)
    
    gen <- newStdGen
    
    _ <- system clearCommand
    threadDelay 700000
    let grid = fillMatrix rows cols gen
    printString $ toLine grid

    threadDelay 700000
    clearCommand <- getClearCommand
    _ <- system clearCommand
    let grid2 = updateMatrix grid
    printString $ toLine grid2

    threadDelay 700000
    clearCommand <- getClearCommand
    _ <- system clearCommand
    let grid3 = updateMatrix grid2
    printString $ toLine grid3

    threadDelay 700000
    clearCommand <- getClearCommand
    _ <- system clearCommand
    let grid4 = updateMatrix grid3
    printString $ toLine grid4

    -- TODO: clear terminal

