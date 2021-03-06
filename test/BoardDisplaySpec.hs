module BoardDisplaySpec ( main, spec ) where

import           Data.Maybe      (catMaybes)
import           System.Random
import           Test.Hspec
import           Test.QuickCheck

import           ArbitraryTypes  ()
import           TestHelpers

import           Board
import           Display
import           Model

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "shows with line breaks" $
    forAllVisibleBoards $ \board ->
      length (lines (show board)) === height board

  it "can represent diffs with maybe values" $
    let bef = Board
          [ [Wall, Wall]
          , [Grass Nothing, Grass (Just player)]
          , [Grass Nothing, Grass Nothing]
          ]
        aft = boardNextTurn MoveDown bef
    in  changedRows bef aft === [ Nothing
                                , Just [Grass Nothing, Grass Nothing]
                                , Just [Grass Nothing, Grass (Just player)]
                                ]
  it "only changes one line max when moving horizontally" $
    property $ \g (Positive w) (Positive h) ->
      forAll (elements [MoveLeft, MoveRight]) $ \direction ->
        let n = numChanged g w h direction
        in  collect n $ n <= 1

  it "changes zero or two lines when moving vertically" $
    property $ \g (Positive w) (Positive h) ->
      forAll (elements [MoveUp, MoveDown]) $ \direction ->
        let n = numChanged g w h direction
        in  collect n $ n == 0 || n == 2

numChanged :: StdGen -> Int -> Int -> Request -> Int
numChanged g w h direction =
  let bef = spawn g player $ createBoard w h
      aft = boardNextTurn direction bef
  in  length (catMaybes $ changedRows bef aft)
