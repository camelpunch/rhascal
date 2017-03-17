{-# OPTIONS_GHC -Wno-orphans #-}

module ArbitraryTypes where

import           System.Random
import           Test.QuickCheck

import           Model

instance Arbitrary Character where
  arbitrary = do
    Positive hp <- arbitrary
    Positive ac <- arbitrary
    c <- arbitrary
    return Character
      { hitPoints = HitPoints hp
      , armourClass = ArmourClass ac
      , piece = Piece c
      }

instance Arbitrary StdGen where
  arbitrary = do
    n <- arbitrary
    return $ mkStdGen n
