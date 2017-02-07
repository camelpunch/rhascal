module Main where

import System.Console.ANSI
import System.Random

import Board

main :: IO ()
main = do
    showCursor
    clearScreen
    setCursorPosition 0 0
    g <- getStdGen
    print $ generateBoard g width height

width :: Int
width = 80

height :: Int
height = 20
