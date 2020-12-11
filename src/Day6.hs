module Day6 where

import Common
import Data.Function ((&))
import Data.List.Split
import Data.List
import qualified Data.Set as Set

solve2 :: Solution
solve2 input =
  lines input
  -- Get groups
  & splitOn [""]
  -- Make each person's answer a Set
  & map (map Set.fromList)
  -- Get the intersection of each person's answers in each group
  & map (foldl Set.intersection (Set.fromList ['a'..'z']))
  -- Count up those questions in each group
  & map Set.size
  -- Sum the values for all groups
  & sum
  & print

solve1 :: Solution
solve1 input =
  lines input
  -- Get groups
  & splitOn [""]
  & map concat
  -- Count unique answers in each group
  & map (length . nub . sort)
  & sum
  & print
