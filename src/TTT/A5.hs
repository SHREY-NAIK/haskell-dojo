module TTT.A5 where

import Control.Monad (when)
import System.Random.Stateful (globalStdGen, uniformM)
import TTT.A1
import TTT.A2
import TTT.A3
import TTT.A4

-- Q#01

printBoard :: Board -> IO ()
printBoard board = putStrLn (formatBoard board)

-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/ttt-logo.txt"

printLogo :: IO ()
printLogo = do
    logo <- readFile _LOGO_PATH_
    putStrLn logo

-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

firstPlayer :: IO Player
firstPlayer = getFirstPlayer <$> _RANDOM_BOOL_

-- Q#04
getMove :: Board -> IO Move
getMove board = do
    putStrLn "Enter your move (e.g., A1):"
    input <- getLine
    let move = stringToMove input
    if isValidMove board move
        then return move
        else do
            putStrLn "Invalid move. Try again."
            getMove board
-- Q#05

play :: Board -> Player -> IO ()
play board player = do
    when _DISPLAY_LOGO_ printLogo
    putStrLn $ formatBoard board
    putStrLn $ promptPlayer player
    move <- getMove board
    let (gameState, newBoard) = playMove player board move
    case gameState of
        InProgress -> play newBoard (switchPlayer player)
        _ -> do
            putStrLn $ formatBoard newBoard
            putStrLn $ showGameState gameState

-- Q#06

runTTT :: IO ()
runTTT = do
    when _DISPLAY_LOGO_ printLogo
    first <- firstPlayer
    let runGame = play _EMPTY_BOARD_
    runGame first

-- Q#07

--printLogo :: IO ()
--printLogo = do
   -- logoContent <- readFile _LOGO_PATH_
  --  putStrLn logoContent

--firstPlayer :: IO Player
--firstPlayer = do
  --  let randomBool = _RANDOM_BOOL
 --   return $ getFirstPlayer randomBool





-- Q#09

--getMove :: Board -> IO Move
--getMove board = do
--    putStrLn "Enter your move (e.g., A1):"
 --   userInput <- getLine
 --   let move = stringToMove userInput
 --   if isValidMove board move
 --       then return move
 --       else do
  --          putStrLn "Invalid move! Please try again."
  --          getMove board

-- Q#10

--play :: Board -> Player -> IO ()
--play board player = do
 --   when _DISPLAY_LOGO_ printLogo
 --   putStrLn $ formatBoard board
 --   putStrLn $ promptPlayer player
  --  move <- getMove board
  --  let (newGameState, newBoard) = playMove player board move
  --  case newGameState of
  --      InProgress -> play newBoard (switchPlayer player)
  --      _ -> do
   --         putStrLn $ formatBoard newBoard
   --         putStrLn $ showGameState newGameState