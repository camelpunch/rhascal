module RequestHandling
    ( nextTurn
    ) where

import           Board
import           Model

nextTurn :: Request -> Board -> Board
nextTurn request b =
  case request of
    MoveLeft  -> left isPlayer b
    MoveRight -> right isPlayer b
    MoveUp    -> up isPlayer b
    MoveDown  -> down isPlayer b
    DoNothing -> b
  where
    isPlayer c = piece c == Piece '@'
