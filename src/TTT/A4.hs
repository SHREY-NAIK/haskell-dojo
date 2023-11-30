module TTT.A4 where

import Data.List (transpose)
import TTT.A1
import TTT.A2
import TTT.A3 (getAllLines, putSquare)

-- Q#01

_HEADER_ :: String
_HEADER_ = ' ' : formatLine (map show [0.._SIZE_ - 1])

showSquares :: [Square] -> [String]
showSquares squares = map showSquare squares

dropFirstCol :: Board -> Board
dropFirstCol = map (drop 1)


dropLastCol :: Board -> Board
dropLastCol = map reverse . dropFirstCol . map reverse



--Q#05
formatRows :: [Row] -> [String]
formatRows rows = map (\row -> formatLine (showSquares row)) rows


-- Q#06

isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ player line =
  not (null line) &&
  null (filter (/= player) line)

-- Q#07
isWinningLine :: Player -> Line -> Bool
isWinningLine player line =
  not (null line) &&
  foldr (\square acc -> square == player && acc) True line

-- Q#08

hasWon :: Player -> Board -> Bool
hasWon player board =
  let allLines = getAllLines board
  in foldr (\line acc -> acc || isWinningLine player line) False allLines

_X_WIN_ :: Board
_X_WIN_ = [ [X, O, O]
          , [O, X, O]
          , [O, O, X]
          ]

_O_WIN_ :: Board
_O_WIN_ = [ [O, X, O]
          , [X, X, O]
          , [X, O, O]
          ]

-- Q#09
getGameState :: Board -> GameState
getGameState board
  | hasWon X board = XWon
  | hasWon O board = OWon
  | isTied board = Tie
  | otherwise = InProgress
  where
    isTied :: Board -> Bool
    isTied = all (\row -> all (/= Empty) row)

playMove :: Player -> Board -> Move -> (GameState, Board)
playMove player board move =
  let updatedBoard = putSquare player board move
  in (getGameState updatedBoard, updatedBoard)



-- Q#10
prependRowIndices :: [String] -> [String]
prependRowIndices rows = zipWith (\index str -> index : ' ' : str) ['A'..] rows



-- Q#11
formatBoard :: Board -> String
formatBoard = unlines . (\x ->  _HEADER_ : x) . prependRowIndices . formatRows