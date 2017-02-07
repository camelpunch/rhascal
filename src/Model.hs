module Model
    ( ArmourClass(..)
    , Character(..)
    , HitPoints(..)
    , Piece(..)
    , Point(..)
    , Board(..)
    , Tile(..)
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
    , coords :: Point
    } deriving (Eq, Show)

newtype Board =
    Board [[Tile]]

instance Show Board where
    show (Board board) =
        foldl
            (\output row ->
                 output ++
                 map
                     (\tile ->
                          case tile of
                              Grass character -> piece2char $ piece character
                              Wall -> '#')
                     row ++
                 "\n")
            ""
            board

piece2char :: Piece -> Char
piece2char (Piece char) = char

data Tile
    = Grass Character
    | Wall
    deriving (Eq, Show)
