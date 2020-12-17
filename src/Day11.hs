{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}

module Day11 where

import Data.List
import Common
import Data.Maybe
import Graphics.Vty
import Control.Monad

type Coords = (Int, Int)
type BoardDirection = (Int, Int)
type Board = [[Seat]]

data Seat = Floor | Empty | Occupied
  deriving Eq

instance Show Seat where
  show Floor = "."
  show Empty = "L"
  show Occupied = "#"

readSeat '.' = Floor
readSeat 'L' = Empty
readSeat '#' = Occupied

readBoard :: String -> Board
readBoard = map (map readSeat) . lines

showBoard :: Board -> String
showBoard = concat . intercalate ["\n"] . map (map show)

bAt :: Board -> Coords -> Seat
board `bAt` (row, col) = board !! row !! col

validCoords :: Board -> Coords -> Bool
validCoords board (row, col) =
  row >= 0
  && col >= 0
  && row < length board
  && col < length (head board)

addCoords :: Coords -> Coords -> Coords
addCoords (a, b) (x, y) = (a+x, b+y)

multCoords :: Int -> Coords -> Coords
multCoords n (x, y) = (n*x, n*y)

neighbourCoords :: Board -> Coords -> [Coords]
neighbourCoords board (row, col) =
  filter (validCoords board)
  [ (row+1, col)
  , (row-1, col)
  , (row, col+1)
  , (row, col-1)
  , (row-1, col-1)
  , (row-1, col+1)
  , (row+1, col-1)
  , (row+1, col+1)
  ]

--- Part 1

stepP1 :: Board -> Board
stepP1 board =
  indexedMap (\row ->
  indexedMap (\col state ->
    let
      coords = (row, col)
      neighbours = map (board `bAt`) (neighbourCoords board coords)
      occupiedNeighbours = count Occupied neighbours
    in case state of
      Empty | occupiedNeighbours == 0 -> Occupied
      Occupied | occupiedNeighbours >= 4 -> Empty
      _ -> state))
  board

solve1 = print @Int . sum @[] . map (count Occupied) . untilFixed stepP1 . readBoard

--- Part2

directions :: [BoardDirection]
directions =
  [ (-1, -1)
  , (-1,  0)
  , (-1,  1)
  , ( 0, -1)
  , ( 0,  1)
  , ( 1, -1)
  , ( 1,  0)
  , ( 1,  1)
  ]

fullRaycast :: Board -> Coords -> BoardDirection -> [Seat]
fullRaycast board start direction =
  map (board `bAt`)
  $ takeWhile (validCoords board)
  $ map (addCoords start . (`multCoords` direction)) [1..]

raycast
  :: Board
  -> Coords
  -> BoardDirection -- ^
  -> Seat -- ^ Floor means it hit the end
raycast board start direction =
  fromMaybe Floor
  $ find (/= Floor)
  $ fullRaycast board start direction

stepP2 :: Board -> Board
stepP2 board =
  indexedMap (\row ->
  indexedMap (\col state ->
    let
      coords = (row, col)
      visible = map (raycast board coords) directions
      visibleOccupied = count Occupied visible
    in case state of
      Empty | visibleOccupied == 0 -> Occupied
      Occupied | visibleOccupied >= 5 -> Empty
      _ -> state))
  board

solve2 = print @Int . sum @[] . map (count Occupied) . untilFixed stepP2 . readBoard

-- Visualization

visualizeForStep :: (Board -> Board) -> Extra
visualizeForStep stepFun (readBoard -> board) =
  mainLoop stepFun showBoard board
  where
    showBoard :: Board -> Image
    showBoard = vertCat . map (string mempty . concatMap show)

extras :: Extras
extras = [("viz1", visualizeForStep stepP1), ("viz2", visualizeForStep stepP2)]
