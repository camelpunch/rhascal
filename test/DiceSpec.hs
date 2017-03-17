module DiceSpec ( main, spec ) where

import           System.Random
import           Test.Hspec
import           Test.QuickCheck

import           TestHelpers

import           Dice

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "d4" $
    it "is within range" $ property $ \seed n ->
      let rolls = take n $ rollsOf (D4 $ mkStdGen seed)
      in  all (`elem` [1..4]) rolls

  describe "d20" $
    it "is within range" $ property $ \seed n ->
      let rolls = take n $ rollsOf (D20 $ mkStdGen seed)
      in  all (`elem` [1..20]) rolls
