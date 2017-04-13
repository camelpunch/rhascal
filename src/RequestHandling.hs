module RequestHandling
    ( nextTurn
    ) where

import           Board
import           Model

nextTurn :: Request -> Turn -> Turn
nextTurn request turn =
  case request of
    MoveLeft  -> Turn (direction left)  Movement
    MoveRight -> Turn (direction right) Movement
    MoveUp    -> Turn (direction up)    Movement
    MoveDown  -> Turn (direction down)  Movement
    DoNothing -> Turn b                 NoChange
  where
    b = boardAfter turn
    direction f = f (== player) b
