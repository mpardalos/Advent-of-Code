module Day5 where

import Data.Functor ((<&>))
import Data.Bifunctor (first, bimap)
import Debug.Trace
import GHC.IO
import Data.List
import Data.Function ((&))

type Row = Int
type Column = Int

maxRow :: Row
maxRow = 127

maxColumn :: Column
maxColumn = 7

data Direction = Up | Down

type RowDirections = [Direction]
type ColumnDirections = [Direction]

parseBoardingPass :: String -> ([Direction], [Direction])
parseBoardingPass =
  bimap (map readRowDir) (map readColDir) .
  (splitAt 7)
  where
    readRowDir 'F' = Down
    readRowDir 'B' = Up
    readRowDir c = error (show c)

    readColDir 'L' = Down
    readColDir 'R' = Up
    readColDir c = error (show c)

decodeBoardingPass :: (RowDirections, ColumnDirections) -> (Row, Column)
decodeBoardingPass = bimap (decodeDirections maxRow) (decodeDirections maxColumn)
  where
    midpoint :: Int -> Int -> Double
    midpoint l h = ((fromIntegral h) + (fromIntegral l)) / 2

    decodeDirections :: Int -> [Direction] -> Int
    decodeDirections lim = fst . foldl
        (\(l, h) dir ->  case dir of
                Up -> (ceiling (midpoint l h), h)
                Down -> (l, floor (midpoint l h))
        )
        (0::Int, lim)

seatId :: (Row, Column) -> Int
seatId (r, c) = 8 * r + c

pairs :: [a] -> [(a, a)]
pairs (x1:x2:xs) = (x1,x2) : pairs (x2:xs)
pairs _ = []

findMissing :: [Int] -> Int
findMissing =
  (+1)
  . fst
  . (!!0)
  . filter (\(x,y) -> y /= (x+1))
  . pairs

solve :: String -> IO ()
solve = solve2

solve1 :: String -> IO ()
solve1 input =
  lines input
  & fmap parseBoardingPass
  & fmap decodeBoardingPass
  & fmap seatId
  & maximum
  & print

solve2 :: String -> IO ()
solve2 input =
  lines input
  & fmap parseBoardingPass
  & fmap decodeBoardingPass
  & fmap seatId
  & sort
  & findMissing
  & print
