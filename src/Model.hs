module Model
    ( ArmourClass(..)
    , Character(..)
    , HitPoints(..)
    , Piece(..)
    , Point(..)
    ) where

newtype Piece =
    Piece Char
    deriving (Eq, Show)

newtype HitPoints =
    HitPoints Int
    deriving (Eq, Show)

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
