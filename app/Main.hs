module Main where

import System.Console.ANSI
import System.IO
import System.Random

import Board
import Model
import RequestHandling

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    showCursor
    setCursorPosition 0 0
    g <- getStdGen
    loop $ generateBoard g width height

loop :: Board -> IO ()
loop board = do
    clearScreen
    putStrLn ""
    print board
    key <- getChar
    case key of
        'h' -> do
            loop $ handleRequest MoveLeft board
        'j' -> do
            loop $ handleRequest MoveDown board
        'k' -> do
            loop $ handleRequest MoveUp board
        'l' -> do
            loop $ handleRequest MoveRight board
        _ -> loop board

width :: Int
width = 80

height :: Int
height = 20
