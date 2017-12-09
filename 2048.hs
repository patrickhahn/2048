import Data.List
import Data.List.Split
import System.Random

main = do
    gen <- getStdGen
    let (board, newGen) = insertNewTile gen [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
    let (newBoard, newNewGen) = insertNewTile newGen board
    putStrLn $ showBoard newBoard
    playTurn newBoard newNewGen

playTurn board gen = do
    dir <- getLine
    if null dir
        then return ()
        else do
            let (newBoard, newGen) = update dir board gen
            putStrLn $ showBoard newBoard
            playTurn newBoard newGen

update dir board gen = case dir of
    "left" -> updateBoard 0 board gen
    "up" -> updateBoard 1 board gen
    "right" -> updateBoard 2 board gen
    "down" -> updateBoard 3 board gen

updateBoard rotations board gen
    | board == newBoard = (board, gen)
    | otherwise = insertNewTile gen newBoard
    where newBoard = turnAndShiftBoard rotations board

turnAndShiftBoard times board = iterate rotateBack (shiftBoard (iterate rotate board !! times)) !! times

shiftBoard board = map (zeropad . shift . filter (/=0)) board

shift (x:y:rest)
    | x == y = x+y:(shift rest)
    | otherwise = x:(shift (y:rest))
shift x = x

zeropad row
    | length row == 4 = row
    | otherwise = row ++ replicate (4 - length row) 0

insertNewTile gen board = replaceZero index board newGen
    where (index, newGen) = randomR (0, (getNumZeros board)-1) gen :: (Int, StdGen)

getNumZeros board = sum $ map (length . filter (==0)) board

replaceZero index board gen = (chunksOf 4 flattened, newGen)
    where
        (flattened, newGen) = replaceIndex (nthZero index boardList) boardList gen
        boardList = concat board

replaceIndex :: Int -> [Int] -> StdGen -> ([Int], StdGen)
replaceIndex n xs gen = (take n xs ++ [newTile] ++ drop (n + 1) xs, newGen)
    where (newTile, newGen) = get2or4 gen

nthZero n list = (elemIndices 0 list) !! n

get2or4 :: StdGen -> (Int, StdGen)
get2or4 gen
    | 1 == rand = (4, newGen)
    | otherwise = (2, newGen)
    where (rand, newGen) = randomR (1,10) gen :: (Int, StdGen)

rotate :: [[Int]] -> [[Int]]
rotate board
    | (length $ head board) == 1 = [map (head) board]
    | otherwise = (map (last) board):(rotate (map (init) board))

rotateBack :: [[Int]] -> [[Int]]
rotateBack board
    | length (head board) == 1 = [reverse (map (head) board)]
    | otherwise = (reverse (map (head) board)):(rotateBack (map (tail) board))

showBoard :: [[Int]] -> String
showBoard board = unlines $ map show board
