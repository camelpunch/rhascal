module Combat
    ( landing
    , damage
    , Roll
    , Landing(..)
    , Defender
    , module Model
    ) where

import Dice
import Model

type Defender = Character

data Landing
    = Hit
    | CriticalHit
    | Miss
    | Invalid
    deriving (Eq, Show)

landing :: Roll -> Defender -> Landing
landing 1 _ = Miss
landing 20 _ = CriticalHit
landing n x
    | n > 20 = Invalid
    | ArmourClass n >= armourClass x = Hit
    | otherwise = Miss

damage
    :: Die a
    => a -> Landing -> Defender -> Defender
damage die l x =
    x
    { hitPoints =
          case l of
              Hit -> HitPoints (initial - roll1)
              CriticalHit -> HitPoints (initial - roll1 - roll2)
              Miss -> HitPoints initial
              Invalid -> HitPoints initial
    }
  where
    (roll1:roll2:_) = rollsOf die ++ repeat 0
    HitPoints initial = hitPoints x
