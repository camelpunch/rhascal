module Main where

import System.Console.ANSI

import Model

main :: IO ()
main = do
    showCursor
    clearScreen
    setCursorPosition 0 0
    putStrLn $ replicate width '#'
    print
        Character
        { piece = Piece '@'
        , coords = Point (0, 0)
        , hitPoints = HitPoints 12
        , armourClass = ArmourClass 12
        }

width :: Int
width = 80

height :: Int
height = 20
