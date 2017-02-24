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

printChanges :: [Maybe [Tile]] -> IO ()
printChanges = traverse_ put
  where
    put (Just row) = putStrLn $ showRow row
    put Nothing = cursorDownLine 1

printTurns :: [Board] -> IO ()
printTurns turns = do
    setCursorPosition height 0
    putStrLn $ "Turn: " ++ show (length turns)

width :: Int
width = 80

height :: Int
height = 20
