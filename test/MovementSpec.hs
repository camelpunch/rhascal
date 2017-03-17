module MovementSpec ( main, spec ) where

import           Test.Hspec
import           Test.QuickCheck

import           Board
import           Combat
import           RequestHandling

import           ArbitraryTypes  ()
import           TestHelpers

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "manual movement (usually a player)" $ do
    specify "movement in every direction ends back at start" $
      forAllVisibleBoards $ \bef ->
        let aft = allDirections bef
        in  hasSpaceToMoveLeft bef && hasSpaceToMoveUp bef ==>
            bef === aft

    specify "moving left moves player left" $ property $ \g (Positive w) (Positive h) ->
      let bef = generateBoard g w h
          aft = nextTurn MoveLeft bef
      in  boardCounterexample bef aft $ hasSpaceToMoveLeft bef ==>
          playerX aft === playerX bef - 1

    specify "moving up moves player up" $ property $ \g (Positive w) (Positive h) ->
      let bef = generateBoard g w h
          aft = nextTurn MoveUp bef
      in  boardCounterexample bef aft $ hasSpaceToMoveUp bef ==>
          playerY aft === playerY bef - 1
