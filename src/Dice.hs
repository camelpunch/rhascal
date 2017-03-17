module Dice
    ( Die
    , Roll
    , rollsOf
    , D4(..)
    , D20(..)
    ) where

import           System.Random

type Roll = Int

class Die a where
  rollsOf :: a -> [Roll]

newtype D4 =
  D4 StdGen

instance Die D4 where
  rollsOf (D4 g) = randomRs (1, 4) g

newtype D20 =
  D20 StdGen

instance Die D20 where
  rollsOf (D20 g) = randomRs (1, 20) g
