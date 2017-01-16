module Combat
    ( roll
    , landing
    , Roll
    , Landing(..)
    , Defender
    ) where

import System.Random

import Model

type Defender = Character

data Landing
    = Hit
    | CriticalHit
    | Miss
    | Invalid
    deriving (Eq, Show)

type Roll = Int

roll :: Int -> StdGen -> Roll
roll sides g = n
  where
    (n, _) = randomR (1, sides) g

landing :: Roll -> Defender -> Landing
landing 1 _ = Miss
landing 20 _ = CriticalHit
landing n x
    | n > 20 = Invalid
    | ArmourClass n >= armourClass x = Hit
    | otherwise = Miss
