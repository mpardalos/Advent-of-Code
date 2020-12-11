{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}

module Day9 where

import Common
import Data.Function
import Data.Maybe
import Text.Printf
import Data.List

type XMAS = [Int]

addingTo :: Int -> [Int] -> Maybe (Int, Int)
addingTo _ [] = Nothing
addingTo target (x:xs) =
  if (target-x) `elem` xs
  then Just (x, target-x)
  else addingTo target xs

isValidAt :: XMAS -> Int -> Bool
isValidAt xmas n = isJust $ addingTo (xmas!!n) (slice (n-25) n xmas)

invalidValue :: XMAS -> Int
invalidValue xmas =
  fst $ (!!0)
  $ filter (not.snd)
  $ map (\n -> (xmas !! n, xmas `isValidAt` n)) [25..length xmas - 1]

-- | Check if a prefix of the list adds up to a number
blackjack :: Int -> [Int] -> Maybe [Int]
blackjack 0 _ = Just []
blackjack n _ | n < 0 = Nothing
blackjack n [] = Nothing
blackjack n (x:xs) = (x:) <$> blackjack (n-x) xs

solve1 :: Extra
solve1 input =
  lines input
  & map (read @Int)
  & invalidValue
  & printf "Solution: %d\n"

solve2 :: Extra
solve2 input =
  let xmas = map (read @Int) $ lines input
      target = invalidValue xmas
  in mapMaybe (blackjack target) (tails xmas)
     & filter ((>1) . length)
     & (!!0)
     & sort
     & (\xs -> head xs + last xs)
     & printf "Solution: %d\n"
