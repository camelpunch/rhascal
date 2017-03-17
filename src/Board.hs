module Board
    ( generateBoard
    ) where

import           System.Random

import           Model

generateBoard :: StdGen -> Int -> Int -> Board
generateBoard g width height
    | internal width >= 1 && internal height >= 1 =
        generateBoardWithContent g width height
    | otherwise = Board $ replicate height $ replicate width Wall

generateBoardWithContent :: StdGen -> Int -> Int -> Board
generateBoardWithContent g width height =
    Board $ [horzWall width] ++ rows ++ [horzWall width]
  where
    row y =
        [Wall] ++
        take
            (internal width)
            (rowItems (choosePlayerPoint (internal width) (internal height)) y) ++
        [Wall]
    rows = map row [1 .. internal height]
    rowItems playerPoint y = map (\x -> newTile playerPoint (x, y)) [1 ..]
    newTile playerPoint candidatePoint =
        if playerPoint == candidatePoint
            then Grass (Just newPlayer)
            else Grass Nothing
    choosePlayerPoint w h = (playerX, playerY)
      where
        (playerX, g') = randomR (1, w) g
        (playerY, _) = randomR (1, h) g'

horzWall :: Int -> [Tile]
horzWall width = replicate width Wall

newPlayer :: Character
newPlayer =
    Character
    {piece = Piece '@', hitPoints = HitPoints 1, armourClass = ArmourClass 1}

internal :: Int -> Int
internal n = n - 2
