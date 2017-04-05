module Board
  ( generateBoard
  , spawn
  , count
  , width
  , height
  , left
  , right
  , up
  , down
  ) where

import           Data.List
import           Data.Matrix   (fromLists, getElem, setElem, toLists)
import           System.Random

import           Model

generateBoard :: Int -> Int -> Board
generateBoard w h = Board generateRows where

  generateRows
    | internal w >= 1 && internal h >= 1
    = [horzWall] ++ rows ++ [horzWall]
    | otherwise
    = replicate h horzWall

  horzWall = replicate w Wall

  rows = map (const row) [1 .. internal h]

  row = [Wall] ++ internalRow ++ [Wall]

  internalRow = replicate (internal w) $ Grass Nothing

spawn :: StdGen -> Character -> Board -> Board
spawn _ _ b@(Board [_]) = b
spawn _ _ b@(Board ([_]:_)) = b
spawn g c b =
  case getTile (x, y) b of
    Wall           -> b
    Grass (Just _) -> tryAgain
    Grass Nothing  -> setTile (Grass $ Just c) (x, y) b
  where
    (x, g') = randomR (1, internal $ width  b) g
    (y, _ ) = randomR (1, internal $ height b) g'
    tryAgain = spawn (snd $ next g) c b

getTile :: Point -> Board -> Tile
getTile (x, y) (Board rows) =
  getElem (y + 1) (x + 1) (fromLists rows)

setTile :: Tile -> Point -> Board -> Board
setTile t (x, y) (Board rows) =
  Board $ toLists $ setElem t (y + 1, x + 1) $ fromLists rows

internal :: Int -> Int
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
