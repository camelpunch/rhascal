module Board
    ( generateBoard
    ) where

import           System.Random

import           Model

generateBoard :: StdGen -> Int -> Int -> Board
generateBoard g width height
  | internal width >= 1 && internal height >= 1 =
      generateBoardWithContent g width height
  | otherwise =
      Board $ replicate height $ replicate width Wall

generateBoardWithContent :: StdGen -> Int -> Int -> Board
generateBoardWithContent g width height =
  Board $ [horzWall width] ++ rows ++ [horzWall width]
  where
    row y = [Wall] ++ internalRow y ++ [Wall]
    internalRow y =
      take (internal width) $
           rowItems (choosePlayerPoint g (internal width) (internal height))
                     y
    rows = map row [1 .. internal height]

choosePlayerPoint :: StdGen -> Int -> Int -> Point
choosePlayerPoint g w h = (playerX, playerY)
  where
    (playerX, g') = randomR (1, w) g
    (playerY, _) = randomR (1, h) g'

rowItems :: Point -> Int -> [Tile]
rowItems playerPoint y =
  map (\x -> newTile playerPoint (x, y)) [1..]
  where
    newTile point candidatePoint =
      if point == candidatePoint
        then Grass (Just newPlayer)
        else Grass Nothing

horzWall :: Int -> [Tile]
horzWall width = replicate width Wall

newPlayer :: Character
newPlayer = Character { piece = Piece '@'
                      , hitPoints = 1
                      , armourClass = 1}

internal :: Int -> Int
internal n = n - 2
