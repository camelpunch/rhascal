module Board
    ( generateBoard
    ) where

import System.Random

import Model

generateBoard :: StdGen -> Int -> Int -> Board
generateBoard g width height
    | internal width >= 1 && internal height >= 1 =
        let row y =
                [Wall] ++
                rowItems
                    (plotPlayer g (internal width) (internal height))
                    (internal width)
                    y ++
                [Wall]
            rows = map row [1 .. internal height]
        in Board $ [horzWall width] ++ rows ++ [horzWall width]
    | otherwise = Board $ replicate height $ replicate width Wall

plotPlayer :: StdGen -> Int -> Int -> Point
plotPlayer g width height = Point (playerX, playerY)
  where
    (playerX, g') = randomR (1, width) g
    (playerY, _) = randomR (1, height) g'

horzWall :: Int -> [Tile]
horzWall width = replicate width Wall

rowItems :: Point -> Int -> Int -> [Tile]
rowItems playerPoint width y =
    map (\x -> plot playerPoint (Point (x, y))) [1 .. width]

plot :: Point -> Point -> Tile
plot playerPoint candidate =
    if playerPoint == candidate
        then Grass $ Just $ player playerPoint
        else Wall

player :: Point -> Character
player point =
    Character
    { piece = Piece '@'
    , hitPoints = HitPoints 1
    , armourClass = ArmourClass 1
    , coords = point
    }

internal :: Int -> Int
internal n = n - 2
