module Main where

import System.Console.ANSI
import System.IO
import System.Random

import Board
import Model
import RequestHandling

main :: IO ()
main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    showCursor
    setCursorPosition 0 0
    g <- getStdGen
    clearScreen
    loop $ generateBoard g width height

loop :: Board -> IO ()
loop board = do
    cursorUpLine (height + 3)
    print board
    key <- getChar
    cursorUpLine 2
    let action =
            case key of
                'h' -> MoveLeft
                'j' -> MoveDown
                'k' -> MoveUp
                'l' -> MoveRight
                _ -> DoNothing
    loop $ handleRequest action board

width :: Int
width = 80

height :: Int
height = 20
