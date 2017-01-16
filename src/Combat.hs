module Combat
    ( landing
    , damage
    , Roll
    , Landing(..)
    , Defender
    , module Model
    ) where

import Model
import System.Random

type Defender = Character

data Landing
    = Hit
    | CriticalHit
    | Miss
    | Invalid
    deriving (Eq, Show)

type Roll = Int

landing :: Roll -> Defender -> Landing
landing 1 _ = Miss
landing 20 _ = CriticalHit
landing n x
    | n > 20 = Invalid
    | ArmourClass n >= armourClass x = Hit
    | otherwise = Miss

damage :: StdGen -> Landing -> Defender -> Defender
damage g l x = x {hitPoints = causeDamage}
  where
    causeDamage =
        case l of
            Hit -> HitPoints (initial - roll1)
            CriticalHit -> HitPoints (initial - roll1 + roll2)
            Miss -> HitPoints initial
            Invalid -> HitPoints initial
    (roll1:roll2:_) = randomRs d4 g
    HitPoints initial = hitPoints x

d4 :: (Int, Int)
d4 = (1, 4)
