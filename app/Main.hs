module Main where

import           Data.Foldable
import           Data.Function
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
  loop []

loop :: Game -> IO ()
loop [] = do
  g <- getStdGen
  let g' = snd $ next g
      board = createBoard w h &
              spawn g player &
              spawn g' Character { piece = Piece 'j'
                                 , hitPoints = 1
                                 , armourClass = 1
                                 }
      game = [initialTurn board]
  clearScreen
  setCursorPosition 0 0
  print board
  printTurns game
  loop game
  where
    w = 80
    h = 20
loop turns@(previousTurn:_) =
  getChar >>= maybe ignore process . requestFromKey where
    ignore = loop turns

    process request = do
      setCursorPosition 0 0
      printChanges $ changedRows (boardAfter previousTurn) (boardAfter newTurn)
      printTurns (newTurn : turns)
      loop (newTurn : turns)
      where
        newTurn = nextTurn request previousTurn

    printChanges = traverse_ $ maybe skipRow printRow
    skipRow = cursorDownLine 1
    printRow = putStrLn . showRow

printTurns :: Game -> IO ()
printTurns turns = do
  setCursorPosition h 0
  putStrLn $ "Turn: " ++ show (length turns)
  where
    h =
      case turns of
        []    -> 0
        (x:_) -> height $ boardAfter x
