{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Day12 where

import Common
import Debug.Trace

type Distance = Int
data Degrees = D90 | D180 | D270
  deriving (Enum, Eq, Bounded, Show)

data Direction = N | E | S | W
  deriving (Enum, Bounded, Eq, Show)

data Instruction
  = Move Direction Distance
  | F Distance
  | L Degrees
  | R Degrees
  deriving Show

readInput = map readInputLine . lines
  where
    readInputLine :: String -> Instruction
    readInputLine (i:valStr) =
      case (i, read @Int valStr) of
        ('N', value) -> Move N value
        ('S', value) -> Move S value
        ('E', value) -> Move E value
        ('W', value) -> Move W value
        ('L', 90) -> L D90
        ('L', 180) -> L D180
        ('L', 270) -> L D270
        ('R', 90) -> R D90
        ('R', 180) -> R D180
        ('R', 270) -> R D270
        ('F', value) -> F value

data State = State
  { position :: (Int, Int)
  , direction :: Direction
  }
  deriving Show
initState = State (0, 0) E

rotate :: Degrees -> Direction -> Direction
rotate D90 dir = next dir
rotate D180 dir = next (next dir)
rotate D270 dir = next (next (next dir))

rotateMinus :: Degrees -> Direction -> Direction
rotateMinus D90 dir = prev dir
rotateMinus D180 dir = prev (prev dir)
rotateMinus D270 dir = prev (prev (prev dir))

applyInstruction :: State -> Instruction -> State
applyInstruction st@(State pos@(pNorth, pEast) direction) =
  \case
    (Move N val) -> st { position = (pNorth+val, pEast) }
    (Move E val) -> st { position = (pNorth, pEast+val) }
    (Move S val) -> st { position = (pNorth-val, pEast) }
    (Move W val) -> st { position = (pNorth, pEast-val) }
    (L val) -> st { direction = rotateMinus val direction }
    (R val) -> st { direction = rotate val direction }
    (F val) -> applyInstruction st (Move direction val)

solve1 :: Solution
solve1 =
  print @Int
  . (\(n, e) -> abs n + abs e) -- manhattan distance
  . position
  . foldl applyInstruction initState
  . readInput

-- Part 2

data StateP2 = StateP2
  { shipPosition :: (Int, Int)
  , waypointPosition :: (Int, Int)
  }
  deriving Show
initStateP2 = StateP2 (0,0) (1, 10)

applyInstructionP2 :: StateP2 -> Instruction -> StateP2
applyInstructionP2 st@(StateP2 (sNorth, sEast) (wNorth, wEast)) =
  \case
    -- Move the waypoint
    (Move N val) -> st { waypointPosition = ( wNorth+val , wEast     ) }
    (Move E val) -> st { waypointPosition = ( wNorth     , wEast+val ) }
    (Move S val) -> st { waypointPosition = ( wNorth-val , wEast     ) }
    (Move W val) -> st { waypointPosition = ( wNorth     , wEast-val ) }
    -- Rotate the waypoint right
    (R D90)  -> st { waypointPosition = ( -wEast  , wNorth  ) }
    (R D180) -> st { waypointPosition = ( -wNorth , -wEast  ) }
    (R D270) -> st { waypointPosition = ( wEast   , -wNorth ) }
    -- Rotate the waypoint left
    (L D90)  -> st { waypointPosition = ( wEast   , -wNorth ) }
    (L D180) -> st { waypointPosition = ( -wNorth , -wEast  ) }
    (L D270) -> st { waypointPosition = ( -wEast  , wNorth  ) }
    -- Move the ship towards the waypoint
    (F val) -> st { shipPosition = (sNorth + val*wNorth, sEast + val*wEast) }

solve2 :: Solution
solve2 =
  print @Int
  . (\(n, e) -> abs n + abs e) -- manhattan distance
  . shipPosition
  . foldl (\s i -> (show s ++ " | " ++ show i) `trace` applyInstructionP2 s i) initStateP2
  . readInput
  -- $ unlines ["F10", "N3", "F7", "R90", "F11"]
