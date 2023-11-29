module TTT.A2 where

import Data.List (intercalate)
import TTT.A1

-- Q#01

promptPlayer :: Player -> String
promptPlayer player =
  concat ["Player ", show player, "'s turn: enter a row and column position (ex. A1)"]

-- Q#02


_RANGE_ :: [Int]
_RANGE_ = [0.._SIZE_ - 1]

-- Q#03

isDigit :: Char -> Bool
isDigit c = elem c ['0'..'9']

readDigit :: Char -> Int
readDigit c
    | isDigit c = read [c]
    | otherwise = -1

-- Q#04

_EMPTY_ROW_ = replicate _SIZE_ Empty
_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05


isTied :: Board -> Bool
isTied = all (notElem Empty) 

_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
    [X, O, O]
  , [O, X, X]
  , [O, X, O]
  ]

-- Q#06

indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings strings = zip ['A'..] strings

-- Q#07

formatLine :: [String] -> String
formatLine strings = _SEP_ ++ intercalate _SEP_ strings ++ _SEP_

-- Q#08



isMoveInBounds :: Move -> Bool
isMoveInBounds (r, c) = all (\x -> x >= 0 && x < _SIZE_) [r, c]

-- Q#09

stringToMove :: String -> Move
stringToMove [r, c]
    | length [r, c] == 2 = (convertRowIndex r, readDigit c)
stringToMove _ = _INVALID_MOVE_

-- Q#10

replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow player col row
    | col < 0 || col >= length row = row
    | otherwise =
        let (before, after) = splitAt col row
            newAfter = if null after then [] else tail after
            newPiece = case player of
                X -> X : newAfter
                O -> O : newAfter
                Empty -> Empty : newAfter
        in before ++ newPiece

rsX :: Int -> Row -> Row
rsX = replaceSquareInRow X

rsO :: Int -> Row -> Row
rsO = replaceSquareInRow O
