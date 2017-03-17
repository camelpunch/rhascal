module LogicalBoardSpec ( main, spec ) where

import           Test.Hspec
import           Test.QuickCheck

import           ArbitraryTypes  ()
import           TestHelpers

import           Board
import           Model

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  specify "zero height board is empty" $ property $ \g w ->
    generateBoard g w 0 === Board []

  specify "first and last rows are wall" $ property $ \g w (Positive h) ->
    let Board board = generateBoard g w h
        wall = replicate w Wall
    in  head board === wall .&&. last board === wall

  specify "first and last columns are wall" $ property $ \g (Positive w) h ->
    let firstColumn = map head board
        lastColumn = map last board
        Board board = generateBoard g w h
        wall = replicate h Wall
    in  firstColumn === wall .&&. lastColumn === wall

  specify "non-player tiles start empty" $
    forAllVisibleBoards $ \(Board rows) ->
      let countEmpties = length . filter isEmptyTile
          isEmptyTile tile =
            case tile of
              Wall           -> False
              Grass (Just _) -> False
              Grass Nothing  -> True
          h = length rows
          w = length $ head rows
          totalTiles = (w - 2) * (h - 2)
      in  countEmpties (concat rows) === totalTiles - 1

  specify "only one player is spawned" $
    forAllVisibleBoards $ \(Board rows) ->
      let countPlayers = length . filter isPlayer
          isPlayer tile =
            case tile of
              Wall              -> False
              Grass Nothing     -> False
              Grass (Just char) -> piece char == Piece '@'
      in  countPlayers (concat rows) === 1
