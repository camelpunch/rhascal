module Main where

import           Data.Foldable
import           System.Console.ANSI
import           System.IO
import           System.Random

import           Board
import           Display
import           Keyboard
import           Model
import           RequestHandling

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  showCursor
  loop $ Game []

loop :: Game -> IO ()
loop (Game []) = do
  g <- getStdGen
  clearScreen
  let board = generateBoard g w h
      game = Game [board]
  setCursorPosition 0 0
  print board
  printTurns [board]
  loop game
  where
    w = 80
    h = 20
loop (Game turns@(previousTurn:_)) =
  getChar >>= maybe ignore process . requestFromKey
  where
    ignore = loop $ Game turns
    process request = do
      setCursorPosition 0 0
      printChanges $ changedRows previousTurn newTurn
      printTurns (newTurn : turns)
      loop $ Game (newTurn : turns)
      where
        newTurn = nextTurn request previousTurn

printChanges :: [Maybe [Tile]] -> IO ()
printChanges = traverse_ $ maybe skipRow printRow
  where
    skipRow = cursorDownLine 1
    printRow = putStrLn . showRow

printTurns :: [Board] -> IO ()
printTurns turns = do
  setCursorPosition h 0
  putStrLn $ "Turn: " ++ show (length turns)
  where
    h =
      case turns of
        []    -> 0
        (b:_) -> height b
