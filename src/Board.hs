module Board
  ( generateBoard
  , count
  , width
  , height
  ) where

import           System.Random

import           Model

generateBoard :: StdGen -> Int -> Int -> Board
generateBoard g w h
  | internal w >= 1 && internal h >= 1
  = generateBoardWithContent g w h
  | otherwise
  = Board $ replicate h $ replicate w Wall

generateBoardWithContent :: StdGen -> Int -> Int -> Board
generateBoardWithContent g w h =
  Board $ [horzWall w] ++ rows ++ [horzWall w]
  where
    row y = [Wall] ++ internalRow y ++ [Wall]
    internalRow y =
      take (internal w) $
           rowItems (choosePlayerPoint g (internal w) (internal h))
                    y
    rows = map row [1 .. internal h]

choosePlayerPoint :: StdGen -> Int -> Int -> Point
choosePlayerPoint g w h = (playerX, playerY) where
  (playerX, g') = randomR (1, w) g
  (playerY, _g) = randomR (1, h) g'

rowItems :: Point -> Int -> [Tile]
rowItems playerPoint y = map newTile [1..] where
  newTile x
    | (x, y) == playerPoint
    = Grass (Just newPlayer)
    | otherwise
    = Grass Nothing

count :: (Tile -> Bool) -> Board -> Int
count f = length . filter f . tiles

width :: Board -> Int
width (Board rows) = length rows

height :: Board -> Int
height (Board rows) = length $ head rows

tiles :: Board -> [Tile]
tiles (Board rows) = concat rows

horzWall :: Int -> [Tile]
horzWall w = replicate w Wall

newPlayer :: Character
newPlayer = Character { piece = Piece '@'
                      , hitPoints = 1
                      , armourClass = 1}

internal :: Int -> Int
internal n = n - 2
