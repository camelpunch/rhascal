{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

import Data.List
import System.Exit
import System.Random

import Combat

import ArbitraryTypes

-- Combat: deciding whether we hit
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

-- Combat: damaging a character
prop_HitCausesDamage x =
    hitPoints (damage g Hit x) === HitPoints (initial - roll)
  where
    HitPoints initial = hitPoints x
    g = mkStdGen 123
    (roll, _) = randomR (1, 4) g

prop_CriticalHitCausesTwoRollsOfDamage x =
    hitPoints (damage g CriticalHit x) === HitPoints (initial - roll1 + roll2)
  where
    g = mkStdGen 1
    HitPoints initial = hitPoints x
    (roll1, g') = randomR (1, 4) g
    (roll2, _) = randomR (1, 4) g'

prop_MissCausesNoDamage x = hitPoints (damage g Miss x) === hitPoints x
  where
    g = mkStdGen 10

prop_InvalidCausesNoDamage x = hitPoints (damage g Invalid x) === hitPoints x
  where
    g = mkStdGen 999

return []

main :: IO ()
main = do
    result <- $quickCheckAll
    case result of
        True -> exitSuccess
        False -> exitFailure
