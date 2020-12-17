{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}

module Day14 where

import Text.Parsec
import Data.Either
import Control.Monad
import Data.Bits
import Common
import Debug.Trace
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Text.Printf
import Data.Foldable
import Graphics.Vty (Image, vertCat, (<->))
import qualified Graphics.Vty as Vty

type Addr = Int
type Value = Int

data MaskBit = I | O | X
  deriving Show
type Mask = [MaskBit]

data Instruction
  = Write Addr Value
  | Mask Mask

instance Show Instruction where
  show (Write addr val) = printf "mem[0x%X] = 0x%X" addr val
  show (Mask mask) = printf "mask = %s" (concatMap show mask)

readInput :: String -> [Instruction]
readInput = either (error . show) id . runParser parser () ""
  where
    parser = instructionP `sepEndBy` (void endOfLine <|> eof)

    instructionP = try maskP <|> try writeP

    number = read @Int <$> many1 digit

    writeP = do
      string "mem["
      addr <- number
      string "] = "
      value <- number
      return (Write addr value)

    maskP = do
      string "mask = "
      Mask <$> many (choice
                     [ char '1' >> pure I
                     , char '0' >> pure O
                     , char 'X' >> pure X])

applyMask :: Mask -> Int -> Int
applyMask mask val = sum $ indexedMap applyBit $ reverse mask
  where
    applyBit :: Int -> MaskBit -> Int
    applyBit pos I = bit pos
    applyBit _ O = 0
    applyBit pos X | testBit val pos = bit pos
                   | otherwise = 0

type Memory = IntMap Int

runInstruction :: (Mask, Memory) -> Instruction -> (Mask, Memory)
runInstruction (_, mem) (Mask mask) = (mask, mem)
runInstruction (mask, mem) (Write addr val) = (mask, IntMap.insert addr (applyMask mask val) mem)

solve1 =
  print
  . sum
  . IntMap.map toInteger
  . snd
  . foldl runInstruction ([], IntMap.empty)
  . readInput

--- Part 2

applyAddrMask :: Mask -> Int -> [Int]
applyAddrMask mask addr =
  map sum
  $ sequence -- cartesian product (by black magic idk)
  $ indexedMap applyBit
  $ reverse mask
  where
    applyBit :: Int -> MaskBit -> [Int]
    applyBit pos I = [bit pos]
    applyBit pos O | testBit addr pos = [bit pos]
                   | otherwise = [0]
    applyBit pos X = [bit pos, 0]

runInstruction2 :: (Mask, Memory) -> Instruction -> (Mask, Memory)
runInstruction2 (_, mem) (Mask mask) = (mask, mem)
runInstruction2 (mask, mem) (Write addr val) =
  let addrs = applyAddrMask mask addr
  in (mask, foldl (\memAcc addr -> IntMap.insert addr val memAcc) mem addrs)

solve2 =
  print
  . sum
  . IntMap.map toInteger
  . snd
  . foldl runInstruction2 ([], IntMap.empty)
  . readInput


-- Visualize

type MemState = (Mask, Memory)

imageForState :: MemState -> Image
imageForState (mask, memory) =
  vertCat
    (Vty.string mempty (printf "mask: %s" (concatMap show mask))
     : IntMap.elems (IntMap.map (Vty.string mempty)
                     (IntMap.mapWithKey (printf "[0x%X] = 0x%X") memory)))

viz2 :: Extra
viz2 input =
  mainLoop nextIP showState 0
  where
    instrs = readInput input

    states = scanl runInstruction2 ([], IntMap.empty) instrs

    showState n =
      Vty.string mempty (show (instrs !! n))
      <->
      imageForState (states !! n)

    nextIP n | n == (length states - 1) = n
             | otherwise = n+1

extras :: Extras
extras = [("viz2", viz2)]
