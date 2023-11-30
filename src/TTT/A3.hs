module TTT.A3 where
import Data.List (intercalate)
import Data.List (transpose)
import TTT.A1
import TTT.A2

-- Q#01

showInts :: [Int] -> [String]
showInts [] = []
showInts (x:xs) = show x : showInts xs

_HEADER_ :: String
_HEADER_ = let
    _RANGE_ :: [Int]
    _RANGE_ = [0..2]

    formatLine :: [String] -> String
    formatLine strings = "_|_" ++ intercalate "_|_" strings ++ "_|_"

    headerFormat :: String
    headerFormat = formatLine (showInts _RANGE_)
  in headerFormat

-- Q#02


showSquares :: [Square] -> [String]
showSquares [] = []
showSquares (x:xs) = showSquare x : showSquares xs

-- Q#03


formatRows :: [Row] -> [String]
formatRows [] = []
formatRows (r:rs) = formatLine (showSquares r) : formatRows rs

-- Q#04

isColEmpty :: Row -> Int -> Bool
isColEmpty [] _ = True
isColEmpty (s:_) 0 = s == Empty
isColEmpty (_:xs) colIndex = isColEmpty xs (colIndex - 1)

-- Q#05

dropFirstCol :: Board -> Board
dropFirstCol = map (drop 1)

dropLastCol :: Board -> Board
dropLastCol = map reverse . dropFirstCol . map reverse
-- Q#06

getDiag1 :: Board -> Line
getDiag1 [] = []
getDiag1 (row:rows) = case row of
    [] -> []
    (s:_) -> s : getDiag1 (map (drop 1) rows)

getDiag2 :: Board -> Line
getDiag2 [] = []
getDiag2 (row:rows) = case reverse row of
    [] -> []
    (s:_) -> s : getDiag2 (map reverse (map (drop 1) (map reverse rows)))

getAllLines :: Board -> [Line]
getAllLines board = concat [board, transpose board, [getDiag1 board, getDiag2 board]]

-- Q#07



putSquare :: Player -> Board -> Move -> Board
putSquare _ [] _ = []
putSquare player (row:rows) (0, col) = replaceSquareInRow player col row : rows
putSquare player (row:rows) (rowIndex, col) = row : putSquare player rows (rowIndex - 1, col)

-- Q#08



prependRowIndices :: [String] -> [String]
prependRowIndices rows = map (\(index, str) -> index : ' ' : str) (indexRowStrings rows)

-- Q#09

isWinningLine :: Player -> Line -> Bool
isWinningLine _ [] = False
isWinningLine player line = isWinningLineWorker player line False
  where
    isWinningLineWorker :: Player -> Line -> Bool -> Bool
    isWinningLineWorker _ [] acc = acc
    isWinningLineWorker p (x:xs) acc
      | x /= p = False
      | x == p = isWinningLineWorker p xs True

-- Q#10
isValidMove :: Board -> Move -> Bool
isValidMove board move@(rowIndex, colIndex)
  | not (isMoveInBounds move) = False
  | otherwise = isValidMoveWorker board move

isValidMoveWorker :: Board -> Move -> Bool
isValidMoveWorker [] _ = False
isValidMoveWorker (row:rows) (0, colIndex) = isColEmpty row colIndex
isValidMoveWorker (row:rows) (rowIndex, colIndex) = isValidMoveWorker rows (rowIndex - 1, colIndex)