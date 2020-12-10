{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}

module Day8 where

import Common
import Data.Function
import Text.Read (readMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Writer
import Text.Printf

type Program = [Instruction]

data Instruction
  = Nop Int
  | Jmp Int
  | Acc Int
  deriving Show

data ProgramState = ProgramState
  { pc :: Int -- >0
  , acc :: Int
  , visited :: [Int] -- in reverse order
  }
  deriving Show

initProgramState :: ProgramState
initProgramState = ProgramState 0 0 []

-- Part 1

type MonadRun m = MonadState ProgramState m

runInstr :: MonadRun m => Instruction -> m ()
runInstr (Nop _) = modify $ \st -> st { pc = pc st + 1 }
runInstr (Jmp p) = modify $ \st -> st { pc = pc st + p }
runInstr (Acc v) = modify $ \st -> st { pc = pc st + 1, acc = acc st + v }

-- | Return whether
run :: MonadRun m => Program -> m Bool
run prog = do
  ProgramState{pc,visited} <- get
  if pc >= length prog
  then return True -- It exits
  else do
    modify (\st -> st {visited = pc:visited})
    if pc `elem` visited
    then return False
    else runInstr (prog !! pc) >> run prog

readInt :: String -> Maybe Int
readInt ('+':s) = readMaybe s
readInt s = readMaybe s

parseInstr :: String -> Instruction
parseInstr (words -> ["nop", readInt -> (Just val)]) = Nop val
parseInstr (words -> ["jmp", readInt -> (Just val)]) = Jmp val
parseInstr (words -> ["acc", readInt -> (Just val)]) = Acc val
parseInstr s = error ("Invalid instruction: " ++ s)

parseProgram :: String -> Program
parseProgram = map parseInstr . lines

solve :: Solution
solve input =
  lines input
  & map parseInstr
  & run
  & (`execState` initProgramState)
  & acc
  & printf "Solution: %d\n"

--- Part 2

repairAt :: Int -> Program -> Program
repairAt 0 ((Nop v):rest) = Jmp v:rest
repairAt 0 ((Jmp v):rest) = Nop v:rest
repairAt 0 ((Acc v):rest) = Acc v:rest
repairAt n (instr:rest) = instr:repairAt (n-1) rest
repairAt _ [] = error "Index out of range"

solve2 :: Solution
solve2 input =
  let baseProg = parseProgram input
      repairedPrograms = map (\n -> repairAt n baseProg) [0..length baseProg - 1]
      results = map (\p -> runState (run p) initProgramState) repairedPrograms
  in
    filter fst results
    & (!!0)
    & (\(_, ProgramState{acc}) -> acc)
    & printf "Solution: %d\n"

--- Extras

visualize :: Program -> IO ()
visualize prog =
  runUntilLoopViz prog
  & (`runStateT` initProgramState)
  & execWriter
  & \outLines -> do
    putStrLn "--Execution trace--"
    putStrLn "[  acc ]   pc: instruction"
    putStrLn "--------------------------"
    mapM_ putStrLn outLines

type MonadViz m = (MonadRun m, MonadWriter [String] m)

runUntilLoopViz :: forall m. MonadViz m => Program -> m Bool
runUntilLoopViz prog = do
  tellState
  ProgramState{pc,visited} <- get
  if pc >= length prog
  then return True -- It exits
  else do
    modify (\st -> st {visited = pc:visited})
    if pc `elem` visited
    then runInstr (prog !! pc) >> runUntilLoopViz prog
    else return False
  where
    tellState = do
      ProgramState {acc, pc} <- get
      tell ([printf "[%5d ] %4d: %s" acc pc (show $ prog !! pc)]::[String])

extras :: Extras
extras = [("trace", visualize . parseProgram), ("p2", solve2)]
