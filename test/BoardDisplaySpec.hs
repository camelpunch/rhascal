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
import           RequestHandling

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
        aft = nextTurn MoveDown bef
        player = Character
          { piece = Piece '@'
          , hitPoints = 1
          , armourClass = 1
          }
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
  let bef = spawnPlayer g $ generateBoard w h
      aft = nextTurn direction bef
  in  length (catMaybes $ changedRows bef aft)
