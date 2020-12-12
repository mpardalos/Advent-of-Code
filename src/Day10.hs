{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Day10 where

import Data.Function
import Data.List
import Data.Maybe
import Common

differences (x1:x2:xs) = (x2-x1) : differences (x2:xs)
differences _ = []

readInput =
  -- Add the start and end ratings
  (\js -> 0:(maximum js + 3):js)
  . map (read @Int)
  . lines

solve1 input =
  readInput input
  & differences . sort
  & group . sort
  & map (\ds -> (head ds, length ds))
  & (\xs -> fromJust (lookup 1 xs) * fromJust (lookup 3 xs))
  & print

-- | How many ways are there to remove connectors from a sequence of connectors
-- with `n` 1-jolt gaps between them
-- TODO. This is something to do with the choice operator, but no clue what exactly
selections :: Int -> Int
selections 1 = 1
selections 2 = 2
selections 3 = 4
selections 4 = 7
selections n = error ("who knows about " ++ show n ++ "?")

solve2 input =
  readInput input
  & sort
  & differences
  & group
  & filter ((==1) . head)
  & map (selections . length)
  & product
  & print
