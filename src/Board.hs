module Board
  ( generateBoard
  , count
  , width
  , height
  , left
  , right
  , up
  , down
  ) where

import           Data.List
import           System.Random

import           Model

generateBoard :: StdGen -> Int -> Int -> Board
generateBoard g w h = Board generateRows where

  generateRows
    | internal w >= 1 && internal h >= 1
    = [horzWall] ++ rows ++ [horzWall]
    | otherwise
    = replicate h horzWall

  horzWall = replicate w Wall

  rows = map row [1 .. internal h]

  row y = [Wall] ++ internalRow y ++ [Wall]

  internalRow y =
    take (internal w) $
         rowItems chosenPlayerCoords y

  rowItems playerCoords y = map newTile [1..] where
    newTile x
      | (x, y) == playerCoords
      = Grass (Just newPlayer)
      | otherwise
      = Grass Nothing

  chosenPlayerCoords = (x, y) where
    (x, g') = randomR (1, internal w) g
    (y, _g) = randomR (1, internal h) g'

  newPlayer = Character { piece = Piece '@'
                        , hitPoints = 1
                        , armourClass = 1}

  internal = subtract 2

count :: (Tile -> Bool) -> Board -> Int
count f = length . filter f . tiles

width :: Board -> Int
width (Board rows) = maybe 0 (length . fst) $ uncons rows

height :: Board -> Int
height (Board rows) = length rows

tiles :: Board -> [Tile]
tiles (Board rows) = concat rows

left :: (Character -> Bool) -> Board -> Board
left f (Board rows) = Board $ map (moveBackOnAxis f) rows

right :: (Character -> Bool) -> Board -> Board
right f (Board rows) = Board $ map (moveForwardOnAxis f) rows

up :: (Character -> Bool) -> Board -> Board
up f (Board rows) = Board $ (transpose . map (moveBackOnAxis f) . transpose) rows

down :: (Character -> Bool) -> Board -> Board
down f (Board rows) = Board $ (transpose . map (moveForwardOnAxis f) . transpose) rows

moveBackOnAxis :: (Character -> Bool) -> [Tile] -> [Tile]
moveBackOnAxis f xs =
  case xs of
    []      -> xs
    [_]     -> xs
    (x:y:t) -> moveBack [] x y t
  where
    moveBack :: [Tile] -> Tile -> Tile -> [Tile] -> [Tile]
    moveBack processed first second rest =
      case (first, second) of
        (_, Wall)                       -> stop
        (Wall, Grass (Just _))          -> stop
        (Grass (Just _), Grass _)       -> stop
        (Wall, Grass Nothing)           -> nextPair
        (Grass Nothing, Grass Nothing)  -> nextPair
        (Grass Nothing, Grass (Just c)) -> if f c then swap else nextPair
        where
          stop = processed ++ (first:second:rest)
          swap = processed ++ (second:first:rest)
          recur = moveBack (processed ++ [first]) second
          nextPair =
            case rest of
              []                  -> stop
              [_]                 -> stop
              (newSecond:newRest) -> recur newSecond newRest

moveForwardOnAxis :: (Character -> Bool) -> [Tile] -> [Tile]
moveForwardOnAxis f = reverse . moveBackOnAxis f . reverse
