module Model
    ( ArmourClass(..)
    , Character(..)
    , HitPoints(..)
    , Piece(..)
    , Point(..)
    , Board(..)
    , Tile(..)
    , Request(..)
    ) where

newtype Piece =
    Piece Char
    deriving (Eq, Show)

newtype HitPoints =
    HitPoints Int
    deriving (Eq, Show, Ord)

newtype ArmourClass =
    ArmourClass Int
    deriving (Eq, Show, Ord)

newtype Point =
    Point (Int, Int)
    deriving (Eq, Show)

data Character = Character
    { piece :: Piece
    , hitPoints :: HitPoints
    , armourClass :: ArmourClass
    } deriving (Eq, Show)

newtype Board =
    Board [[Tile]]
    deriving (Eq)

instance Show Board where
    show (Board board) =
        "\n" ++
        foldl (\output row -> output ++ map tile2char row ++ "\n") "" board

tile2char :: Tile -> Char
tile2char Wall = '#'
tile2char (Grass Nothing) = '.'
tile2char (Grass (Just character)) = piece2char $ piece character
  where
    piece2char (Piece char) = char

data Tile
    = Grass (Maybe Character)
    | Wall
    deriving (Eq, Show)

data Request
    = MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
