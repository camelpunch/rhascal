module MovementSpec ( main, spec ) where

import           Data.Function
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
      property $ \g -> forAllVisibleBoards $ \b ->
        let bef = spawn g player b
            aft = allDirections bef
        in  hasSpaceToMoveLeft bef && hasSpaceToMoveUp bef ==>
            bef === aft

    specify "moving left moves player left" $ property $ \g g' (Positive w) (Positive h) ->
      let jackal = Character { piece = Piece 'j'
                             , hitPoints = 100
                             , armourClass = 100
                             }
          bef = createBoard w h &
                spawn g player &
                spawn g' jackal
          aft = nextTurn MoveLeft bef
      in  boardCounterexample bef aft $ hasSpaceToMoveLeft bef ==>
          playerX aft === playerX bef - 1

    specify "moving up moves player up" $ property $ \g (Positive w) (Positive h) ->
      let bef = spawn g player $ createBoard w h
          aft = nextTurn MoveUp bef
      in  boardCounterexample bef aft $ hasSpaceToMoveUp bef ==>
          playerY aft === playerY bef - 1
