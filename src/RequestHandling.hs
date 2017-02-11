module RequestHandling
    ( handleRequest
    ) where

import Data.List

import Model

handleRequest :: Request -> Board -> Board
handleRequest request board =
    case request of
        MoveLeft -> movePlayerLeft board
        MoveRight -> movePlayerRight board
        MoveUp -> movePlayerUp board
        MoveDown -> movePlayerDown board
  where
    movePlayerLeft (Board b) = Board $ map decreaseOnAxis b
    movePlayerRight (Board b) =
        Board $ map (reverse . decreaseOnAxis . reverse) b
    movePlayerUp (Board b) =
        Board $ transpose $ map decreaseOnAxis $ transpose b
    movePlayerDown (Board b) =
        Board $
        transpose $ map (reverse . decreaseOnAxis . reverse) $ transpose b
    decreaseOnAxis xs =
        case xs of
            [] -> xs
            [_] -> xs
            (x:y:t) -> decrease [] x y t

decrease :: [Tile] -> Tile -> Tile -> [Tile] -> [Tile]
decrease processed x y t =
    case (x, y) of
        (_, Wall) -> input
        (Wall, Grass (Just _)) -> input
        (Grass (Just _), Grass _) -> input
        (Grass Nothing, Grass (Just _)) -> processed ++ [y, x] ++ t
        (Wall, Grass Nothing) -> nextPair
        (Grass Nothing, Grass Nothing) -> nextPair
  where
    input = processed ++ [x, y] ++ t
    nextPair =
        case t of
            [] -> input
            [z] -> processed ++ [x, y, z]
            (x':t') -> decrease (processed ++ [x]) y x' t'
