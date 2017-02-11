module RequestHandling
    ( handleRequest
    ) where

import Model

handleRequest :: Request -> Board -> Board
handleRequest request board =
    case request of
        MoveLeft -> movePlayerLeft board
        MoveRight -> movePlayerRight board
        MoveUp -> board
        MoveDown -> board
  where
    movePlayerLeft (Board b) = Board $ map moveLeft b
    movePlayerRight (Board b) =
        Board $ map (\row -> reverse $ moveLeft $ reverse row) b
    moveLeft row =
        case row of
            [] -> row
            [_] -> row
            (x:y:t) -> movePlayerLeftInRow [] x y t

movePlayerLeftInRow :: [Tile] -> Tile -> Tile -> [Tile] -> [Tile]
movePlayerLeftInRow processed x y t =
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
            (x':t') -> movePlayerLeftInRow (processed ++ [x]) y x' t'
