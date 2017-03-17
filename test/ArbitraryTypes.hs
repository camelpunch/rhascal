module ArbitraryTypes where

import           Test.QuickCheck

import           Model

instance Arbitrary Character where
    arbitrary = do
        Positive hp <- arbitrary
        Positive ac <- arbitrary
        c <- arbitrary
        return
            Character
            { hitPoints = HitPoints hp
            , armourClass = ArmourClass ac
            , piece = Piece c
            }
