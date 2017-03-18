module Model
    ( ArmourClass
    , Character(..)
    , Attacker
    , Defender
    , HitPoints
    , Piece(..)
    , Point
    , Game(..)
    , Board(..)
    , Tile(..)
    , Request(..)
    , showRow
    ) where

newtype Piece = Piece Char
  deriving (Eq, Show)

type HitPoints = Int
type ArmourClass = Int
type Point = (Int, Int)

type Attacker = Character
type Defender = Character

data Character = Character
  { piece       :: Piece
  , hitPoints   :: HitPoints
  , armourClass :: ArmourClass
  } deriving (Eq, Show)

newtype Game = Game [Board]
newtype Board = Board [[Tile]] deriving (Eq)

instance Show Board where
  show (Board board) =
    foldl (\output row -> output ++ showRow row ++ "\n") "" board

showRow :: [Tile] -> String
showRow = map tile2char

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
  | DoNothing
  deriving (Show)
