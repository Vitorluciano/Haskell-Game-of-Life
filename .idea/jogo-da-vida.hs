import Control.Monad

-- Function to create an n x m matrix filled with zeroes
createMatrix :: Int -> Int -> [[Int]]
createMatrix n m = replicate n (replicate m 0)

-- Function to print a matrix
printMatrix :: [[Int]] -> IO ()
printMatrix matrix = forM_ matrix $ \row -> do
    forM_ row $ \cell -> putStr (show cell ++ " ")
    putStrLn ""

main :: IO ()
main = do
    putStrLn "Enter the number of rows (n):"
    n <- readLn
    putStrLn "Enter the number of columns (m):"
    m <- readLn

    let matrix = createMatrix n m
    putStrLn "Matrix:"
    printMatrix matrix
