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
  = Nop
  | Jmp Int
  | Acc Int
  deriving Show

data ProgramState = ProgramState
  { pc :: Int -- >0
  , acc :: Int
  , visited :: Set Int
  }
  deriving Show

type MonadRun m = MonadState ProgramState m

runInstr :: MonadRun m => Instruction -> m ()
runInstr Nop = modify $ \st -> st { pc = pc st + 1 }
runInstr (Jmp p) = modify $ \st -> st {pc = pc st + p}
runInstr (Acc v) = modify $ \st -> st { pc = pc st + 1, acc = acc st + v }

runUntilLoop :: MonadRun m => Program -> m ()
runUntilLoop prog = do
  ProgramState{pc,visited} <- get
  unless (pc `elem` visited) $ do
    modify (\st -> st {visited = Set.insert pc visited})
    runInstr (prog !! pc)
    runUntilLoop prog

readInt :: String -> Maybe Int
readInt ('+':s) = readMaybe s
readInt s = readMaybe s

parseInstr :: String -> Instruction
parseInstr (words -> ["nop", _val]) = Nop
parseInstr (words -> ["jmp", readInt -> (Just val)]) = Jmp val
parseInstr (words -> ["acc", readInt -> (Just val)]) = Acc val
parseInstr s = error ("Invalid instruction: " ++ s)

solve :: Solution
solve input =
  lines input
  & map parseInstr
  & runUntilLoop
  & (`execState` ProgramState 0 0 [])
  & pc
  & printf "Solution: %d\n"

--- Extras

visualize :: Extra
visualize input =
  lines input
  & map parseInstr
  & runUntilLoopViz
  & (`runStateT` ProgramState 0 0 [])
  & execWriter
  & \outLines -> do
    putStrLn "--Execution trace--"
    putStrLn "[  acc ]   pc: instruction"
    putStrLn "--------------------------"
    mapM_ putStrLn outLines

type MonadViz m = (MonadRun m, MonadWriter [String] m)

runUntilLoopViz :: forall m. MonadViz m => Program -> m ()
runUntilLoopViz prog = do
  ProgramState{acc,pc,visited} <- get
  tellState
  unless (pc `elem` visited) $ do
    modify (\st -> st {visited = Set.insert pc visited})
    runInstr (prog !! pc)
    runUntilLoopViz prog
  where
    tellState = do
      ProgramState {acc, pc} <- get
      tell [printf "[%5d ] %4d: %s" acc pc (show $ prog !! pc)]

extras :: Extras
extras = [("trace", visualize)]
