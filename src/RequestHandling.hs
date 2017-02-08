module RequestHandling
    ( handleRequest
    ) where

import Model

handleRequest :: Request -> Board -> Board
handleRequest request board =
    case request of
        MoveLeft -> board
        MoveRight -> board
        MoveUp -> board
        MoveDown -> board
