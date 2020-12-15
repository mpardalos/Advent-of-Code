{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Day13 where

import Common
import Debug.Trace
import Data.List.Split
import Data.Maybe
import Text.Read
import Data.Function
import Data.List
import Data.Ord
import Text.Printf

data BusNotes = BusNotes
  { departTime :: Int
  , buses :: [Maybe Int]
  }
  deriving Show

readInput :: String -> BusNotes
readInput input = lines input & \[departTimeStr, busRoutesStr] ->
  BusNotes { departTime = read departTimeStr
           , buses = map readMaybe $ splitOn "," busRoutesStr
           }

waitTime
  :: Int -- ^ Time when you will start waiting
  -> Int -- ^ Bus you want to take
  -> Int
waitTime t b = b - (t`mod`b)


-- Answer: (383,6)
solve1 input =
  let BusNotes{..} = readInput input
  in catMaybes buses
     & map (\bus -> (bus, waitTime departTime bus))
     & minimumBy (comparing snd)
     & print

solve2 :: Solution
solve2 input =
  let BusNotes{..} = readInput input
  in zip buses [0::Int ..]
     & mapMaybe (\case
                    (Nothing, _) -> Nothing
                    (Just b, i) -> Just (b, i))
     & mapM_ (\(b, i) -> printf "%d mod t = %d\n" b i)
