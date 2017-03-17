module RequestHandling
    ( nextTurn
    ) where

import           Data.List

import           Model

nextTurn :: Request -> Board -> Board
nextTurn request (Board b) =
    Board $
    case request of
        MoveLeft  -> left b
        MoveRight -> right b
        MoveUp    -> up b
        MoveDown  -> down b
        DoNothing -> b
  where
    left = map moveBackOnAxis
    right = map moveForwardOnAxis
    up = transpose . map moveBackOnAxis . transpose
    down = transpose . map moveForwardOnAxis . transpose
    moveBackOnAxis xs =
        case xs of
            []      -> xs
            [_]     -> xs
            (x:y:t) -> moveBack [] x y t
    moveForwardOnAxis = reverse . moveBackOnAxis . reverse

moveBack :: [Tile] -> Tile -> Tile -> [Tile] -> [Tile]
moveBack processed first second rest =
    case (first, second) of
        (_, Wall)                       -> stop
        (Wall, Grass (Just _))          -> stop
        (Grass (Just _), Grass _)       -> stop
        (Wall, Grass Nothing)           -> nextPair
        (Grass Nothing, Grass Nothing)  -> nextPair
        (Grass Nothing, Grass (Just _)) -> swap
  where
    stop = processed ++ [first, second] ++ rest
    swap = processed ++ [second, first] ++ rest
    nextPair =
        case rest of
            [] -> stop
            [_] -> stop
            (second':rest') ->
                moveBack (processed ++ [first]) second second' rest'
