import System.Random
import Control.Concurrent (threadDelay)
import System.Info (os)
import System.Process (system)
import System.IO
import Data.Char

alive = 'a'
dead = ' '
zombie = 'z'
delay = 1000000

--FUNCTIONS TO CLEAR TERMINAL AFTER UPDATING THE GRID

-- Get the clear command based on the operating system
getClearCommand :: IO String
getClearCommand = return $ if isWindowsOS then "cls" else "clear"

-- Check if the operating system is Windows
isWindowsOS :: Bool
isWindowsOS = os == "mingw32"

------------------------------------------------------------------

--get all neighbors of a cell, such that i and j are line and column of the cell
getNeighbors :: Int -> Int -> [String] -> String                                                                                                                                          --used equivalence p -> q then ¬p v q
getNeighbors i j matrix = [matrix !! iNgbr !! jNgbr | iNgbr <- [(i - 1) .. (i + 1)], jNgbr <- [(j - 1) .. (j + 1)], iNgbr >= 0 && jNgbr >= 0 && iNgbr < totalRows && jNgbr < totalCols && (iNgbr /= i || jNgbr /= j)]
    where totalRows = length matrix
          totalCols = length (matrix !! 0)

countOcurrencies :: String -> Char -> Int
countOcurrencies list value = sum [if x == value then 1 else 0 | x <- list]

--check cell type and call its correspondent function
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

updateMatrix :: [String] -> [String]
updateMatrix currentMatrix = [ [updateCell (currentMatrix !! i !! j) (getNeighbors i j currentMatrix) | j <- columnsIndexes] | i <- rowsIndexes ]
    where rowsIndexes = [0 .. (length currentMatrix - 1)]
          columnsIndexes = [0 .. (length (currentMatrix !! 0) - 1)]

runGame :: [String] -> Int -> Int -> IO ()
runGame matrix iterations currentIteration
    | currentIteration == 0 = do
        showNewMatrix matrix
        runGame newMatrix iterations (currentIteration + 1)
    | matrix == newMatrix && currentIteration /= iterations = do
        showNewMatrix matrix
        putStrLn ("System stabled after " ++ (show currentIteration) ++ " iterations!")
    | matrix /= newMatrix && currentIteration == iterations = do
        showNewMatrix matrix
        putStrLn ("System stopped after " ++ (show currentIteration) ++ " iterations!")
    | otherwise = do
        showNewMatrix newMatrix
        runGame newMatrix iterations (currentIteration + 1)
    where newMatrix = updateMatrix matrix

showNewMatrix matrix = do
    --clean terminal
     clearCommand <- getClearCommand
     _ <- system clearCommand
     --print new matrix
     putStr $ unlines matrix
     --wait before print the next matrix
     threadDelay delay
     

main = do
    putStrLn "Enter the initial grid file path: "
    filePath <- getLine
    fileContent <- readFile filePath

    putStrLn "Enter the number of iterations: "
    numIterations <- getLine
    let iterations = (read numIterations :: Int)
    
    let initialGrid = lines fileContent
    runGame initialGrid iterations 0


