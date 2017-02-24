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
loop (Game turns@(x:_)) = do
    key <- getChar
    case requestFromKey key of
        Nothing -> loop $ Game turns
        Just request ->
            let new = handleRequest request x
                newTurns = new : turns
            in do setCursorPosition 0 0
                  printChanges $ changedLines x new
                  printTurns newTurns
                  loop $ Game newTurns

printChanges :: [(Bool, [Tile])] -> IO ()
printChanges = traverse_ put
  where
    put (True, row) = putStrLn $ showRow row
    put (False, _) = cursorDownLine 1

printTurns :: [Board] -> IO ()
printTurns turns = do
    setCursorPosition height 0
    putStrLn $ "Turn: " ++ show (length turns)

width :: Int
width = 80

height :: Int
height = 20
