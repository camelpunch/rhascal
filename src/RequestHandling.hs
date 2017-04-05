module RequestHandling
    ( nextTurn
    ) where

import           Board
import           Model

nextTurn :: Request -> Board -> Board
nextTurn request b =
  case request of
    MoveLeft  -> left  (== player) b
    MoveRight -> right (== player) b
    MoveUp    -> up    (== player) b
    MoveDown  -> down  (== player) b
    DoNothing -> b
