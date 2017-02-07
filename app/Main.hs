module Main where

import System.Console.ANSI
import System.IO
import System.Random

import Board

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    showCursor
    setCursorPosition 0 0
    loop

loop :: IO ()
loop = do
    g <- newStdGen
    clearScreen
    putStrLn ""
    print $ generateBoard g width height
    key <- getChar
    case key of
        'j' -> loop
        _ -> loop

width :: Int
width = 80

height :: Int
height = 20
