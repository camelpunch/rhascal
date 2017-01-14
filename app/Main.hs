module Main where

import Model

main :: IO ()
main =
    print
        Character
        { piece = Piece '@'
        , coords = Point (0, 0)
        , hitPoints = HitPoints 12
        , armourClass = ArmourClass 12
        }
