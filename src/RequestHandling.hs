module RequestHandling
    ( handleRequest
    ) where

import Data.List

import Model

handleRequest :: Request -> Board -> Board
handleRequest request (Board b) =
    Board $
    case request of
        MoveLeft -> left b
        MoveRight -> right b
        MoveUp -> up b
        MoveDown -> down b
        DoNothing -> b
  where
    left = map moveBackOnAxis
    right = map moveForwardOnAxis
    up = transpose . map moveBackOnAxis . transpose
    down = transpose . map moveForwardOnAxis . transpose
    moveBackOnAxis xs =
        case xs of
            [] -> xs
            [_] -> xs
            (x:y:t) -> moveBack [] x y t
    moveForwardOnAxis = reverse . moveBackOnAxis . reverse

moveBack :: [Tile] -> Tile -> Tile -> [Tile] -> [Tile]
moveBack processed x y t =
    case (x, y) of
        (_, Wall) -> stop
        (Wall, Grass (Just _)) -> stop
        (Grass (Just _), Grass _) -> stop
        (Wall, Grass Nothing) -> nextPair
        (Grass Nothing, Grass Nothing) -> nextPair
        (Grass Nothing, Grass (Just _)) -> swap
  where
    stop = processed ++ [x, y] ++ t
    swap = processed ++ [y, x] ++ t
    nextPair =
        case t of
            [] -> stop
            [_] -> stop
            (y':t') -> moveBack (processed ++ [x]) y y' t'
