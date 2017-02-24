module Main where

import Data.Foldable
import System.Console.ANSI
import System.IO
import System.Random

import Board
import Display
import Keyboard
import Model
import RequestHandling

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
    let board = generateBoard g width height
        game = Game [board]
    setCursorPosition 0 0
    print board
    printTurns [board]
    loop game
  where
    width = 80
    height = 20
loop (Game turns@(previousTurn:_)) =
    getChar >>= maybe ignore process . requestFromKey
  where
    process request =
        let newTurn = nextTurn request previousTurn
        in do setCursorPosition 0 0
              printChanges $ changedLines previousTurn newTurn
              printTurns (newTurn : turns)
              loop $ Game (newTurn : turns)
    ignore = loop $ Game turns
    printChanges = traverse_ $ maybe (cursorDownLine 1) (putStrLn . showRow)

printTurns :: [Board] -> IO ()
printTurns turns = do
    setCursorPosition height 0
    putStrLn $ "Turn: " ++ show (length turns)
  where
    height =
        case turns of
            [] -> 0
            (Board b:_) -> length b
