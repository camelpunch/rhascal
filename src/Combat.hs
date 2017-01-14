module Combat
    ( landing
    , Landing(..)
    , Defender
    ) where

import Model

type Defender = Character

data Landing
    = Hit
    | CriticalHit
    | Miss
    deriving (Eq, Show)

type Roll = Int

landing :: Roll -> Defender -> Landing
landing 1 _ = Miss
landing 20 _ = CriticalHit
landing n x
    | ArmourClass n >= armourClass x = Hit
    | otherwise = Miss
