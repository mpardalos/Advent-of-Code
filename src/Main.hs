{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Day4
import qualified Day5
import Text.Read (readMaybe)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.Directory (doesFileExist)

type Solution = String -> IO ()

unsolved :: Solution
unsolved _ = putStrLn "There is no solution for this problem"

solutionFor :: Int -> Solution
solutionFor 1 = \_ -> putStrLn "Use the Makefile in Day1/"
solutionFor 4 = Day4.solve
solutionFor 5 = Day5.solve
solutionFor _ = unsolved

usage :: IO ()
usage = putStrLn "Usage: aoc2020 <day> [input_file]"

dayInput :: Int -> String
dayInput n = "inputs/day" ++ show n

parseArgs :: [String] -> IO (Int, String)
parseArgs ((readMaybe @Int -> Just day):rest) =
  let inputPath = case rest of
        [inputPath] -> inputPath
        [] -> dayInput day
  in doesFileExist inputPath >>= \case
    True -> return (day, inputPath)
    False -> usage >> exitWith (ExitFailure 1)
parseArgs _ = usage >> exitWith (ExitFailure 1)

main :: IO ()
main = do
  (day, inputPath) <- parseArgs =<< getArgs
  inputContents <- readFile inputPath
  solutionFor day inputContents