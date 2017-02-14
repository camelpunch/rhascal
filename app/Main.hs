module Main where

import Data.Foldable
import System.Console.ANSI
import System.IO
import System.Random

import Board
import Display
import Model
import RequestHandling

main :: IO ()
main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    showCursor
    g <- getStdGen
    clearScreen
    let board = generateBoard g width height
    setCursorPosition 0 0
    print board
    loop board

loop :: Board -> IO ()
loop old = do
    key <- getChar
    let new = handleRequest (requestFromKey key) old
    setCursorPosition 0 0
    printChanges $ changedLines old new
    loop new

requestFromKey :: Char -> Request
requestFromKey 'h' = MoveLeft
requestFromKey 'j' = MoveDown
requestFromKey 'k' = MoveUp
requestFromKey 'l' = MoveRight
requestFromKey _ = DoNothing

printChanges :: [(Bool, [Tile])] -> IO ()
printChanges changes = traverse_ put changes
  where
    put (True, row) = putStrLn $ showRow row
    put (False, _) = cursorDownLine 1

width :: Int
width = 80

height :: Int
height = 20
