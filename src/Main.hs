{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Common
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14

import qualified Data.Map as Map
import qualified Options.Applicative as Opt
import Control.Applicative
import Data.Maybe

unsolved :: Solution
unsolved _ = putStrLn "There is no solution for this problem"

data Part = Part1 | Part2
newtype Day = Day Int
dayNum (Day n) = n

solutionFor :: Day -> Part -> Solution
solutionFor (Day 1) _ = \_ -> putStrLn "Use the Makefile in Day1/"
solutionFor (Day 4) Part2 = Day4.solve

solutionFor (Day 5) Part1 = Day5.solve1
solutionFor (Day 5) Part2 = Day5.solve2

solutionFor (Day 6) Part1 = Day6.solve1
solutionFor (Day 6) Part2 = Day6.solve2

solutionFor (Day 8) Part1 = Day8.solve1
solutionFor (Day 8) Part2 = Day8.solve2

solutionFor (Day 9) Part1 = Day9.solve1
solutionFor (Day 9) Part2 = Day9.solve2

solutionFor (Day 10) Part1 = Day10.solve1
solutionFor (Day 10) Part2 = Day10.solve2

solutionFor (Day 11) Part1 = Day11.solve1
solutionFor (Day 11) Part2 = Day11.solve2

solutionFor (Day 12) Part1 = Day12.solve1
solutionFor (Day 12) Part2 = Day12.solve2

solutionFor (Day 13) Part1 = Day13.solve1
solutionFor (Day 13) Part2 = Day13.solve2

solutionFor (Day 14) Part1 = Day14.solve1
solutionFor (Day 14) Part2 = Day14.solve2

solutionFor _ _ = unsolved

extrasFor :: Day -> Extras
extrasFor (Day 8) = Day8.extras
extrasFor (Day 11) = Day11.extras
extrasFor (Day 12) = Day12.extras
extrasFor (Day 14) = Day14.extras
extrasFor _ = Map.empty


usage :: IO ()
usage = putStrLn "Usage: aoc2020 <day> [input_file] [args...]"

fileDoesNotExist :: String -> IO ()
fileDoesNotExist fn = putStrLn ("File does not exist: " ++ fn)

data Args = Args
  { day :: Day
  , partOrMethod :: Either Part String
  , specifiedInputFile :: Maybe String
  }
inputFile Args{..} = fromMaybe (dayInput day) specifiedInputFile
dayInput (Day n) = "inputs/day" ++ show n

args :: Opt.Parser Args
args = Args
  <$> (Day <$> Opt.argument Opt.auto (Opt.metavar "DAY"))
  <*> ( Opt.flag' (Left Part1) (Opt.short '1')
        <|> Opt.flag' (Left Part2) (Opt.short '2')
        <|> Right <$> Opt.strOption (Opt.short 'm' <> Opt.long "method")
      )
  <*> optional (Opt.strOption (Opt.metavar "FILE" <> Opt.short 'i' <> Opt.long "input"))

main :: IO ()
main = do
  args <- Opt.execParser (Opt.info (args <**> Opt.helper) Opt.idm)

  inputContents <- readFile (inputFile args)

  case partOrMethod args of
    Left part -> solutionFor (day args) part inputContents
    Right method -> case Map.lookup method (extrasFor (day args)) of
      Just extra -> extra inputContents
      Nothing -> putStrLn ("There is no " ++ method ++ " method for day " ++ show (dayNum (day args)))
