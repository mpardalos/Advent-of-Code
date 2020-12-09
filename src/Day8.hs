{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}

module Day8 where

import Common
import Data.Function
import Text.Read
import Debug.Trace
import Data.Set (Set)
import qualified Data.Set as Set
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

runInstr :: ProgramState -> Instruction -> ProgramState
runInstr st Nop = st
runInstr st (Jmp p) = st { pc = pc st + p }
runInstr st (Acc v) = st { pc = pc st + 1, acc = acc st + v }

runUntilLoop :: ProgramState -> Program -> ProgramState
runUntilLoop st@ProgramState {pc, acc, visited} prog =
  if pc `elem` visited then st
  else let st' = st {visited = Set.insert pc visited}
  in case prog `at` pc of
    Just instr -> runUntilLoop (runInstr st' instr) prog
    Nothing -> error ("Instruction " ++ show pc ++ " does not exist")

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
  & runUntilLoop (ProgramState 0 0 [])
  & print
