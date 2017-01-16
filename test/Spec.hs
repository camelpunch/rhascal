{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

import Data.List
import System.Exit
import System.Random

import Combat
import Model

import ArbitraryTypes

-- Dice
prop_RollIsDifferentForDifferentSeeds = nub rolls /= rolls
  where
    rolls = map (roll 20 . mkStdGen) [1 .. 1000]

prop_RollIsBetween1AndNumberOfSides seed (Positive sides) =
    roll sides (mkStdGen seed) `elem` [1 .. sides]

-- Combat
prop_1IsAMiss x = landing 1 x === Miss

prop_20LandsCriticalHit x = landing 20 x === CriticalHit

prop_2To19IsAHitIfGreaterThanOrEqualToTargetArmourClass x =
    forAll (choose (2, 19)) $ \n ->
        ArmourClass n >= armourClass x ==> landing n x === Hit

prop_2To19IsAMissIfLessThanTargetArmourClass x =
    forAll (choose (2, 19)) $ \n ->
        ArmourClass n < armourClass x ==> landing n x === Miss

prop_21PlusIsInvalid x =
    forAll (choose (21, 10000)) $ \n -> landing n x === Invalid

return []

main :: IO ()
main = do
    result <- $quickCheckAll
    case result of
        True -> exitSuccess
        False -> exitFailure
