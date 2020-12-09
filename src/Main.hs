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
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import Text.Parsec
import Text.Read (readMaybe)
import Data.Maybe
import Control.Monad
import Data.List
import Text.Parsec.Error (errorMessages)
import qualified Data.Map as Map

unsolved :: Solution
unsolved _ = putStrLn "There is no solution for this problem"

solutionFor :: Int -> Solution
solutionFor 1 = \_ -> putStrLn "Use the Makefile in Day1/"
solutionFor 4 = Day4.solve
solutionFor 5 = Day5.solve
solutionFor 6 = Day6.solve
solutionFor 8 = Day8.solve
solutionFor _ = unsolved

extrasFor :: Int -> Extras
extrasFor 8 = Day8.extras
extrasFor _ = Map.empty

usage :: IO ()
usage = putStrLn "Usage: aoc2020 <day> [input_file]"

fileDoesNotExist :: String -> IO ()
fileDoesNotExist fn = putStrLn ("File does not exist: " ++ fn)

type ArgParser = Parsec [String] ()

data Args = Args
  { day :: Int,
    inputFile :: String,
    method :: Maybe String
  }

argsP :: ArgParser Args
argsP = do
  Just day <- readMaybe @Int <$> arg
  method <- optionMaybe (try (arg `matching` stripPrefix "--"))
  inputFile <- arg `orDefault` dayInput day

  return Args{..}
  where
    arg = anyToken
    p `orDefault` def = fromMaybe def <$> optionMaybe p
    dayInput n = "inputs/day" ++ show n
    matching p f = f <$> p >>= \case
      Just val -> pure val
      Nothing -> fail ""

parseArgs = runParser argsP () "args"

main :: IO ()
main = do
  Args {..} <- (parseArgs <$> getArgs) >>= \case
    Right args -> pure args
    Left err -> usage >> exitWith (ExitFailure 1)

  inputContents <- readFile inputFile

  case method of
    Nothing -> solutionFor day inputContents
    Just method -> case Map.lookup method (extrasFor day) of
      Just extra -> extra inputContents
      Nothing -> putStrLn ("There is no " ++ method ++ " method for day " ++ show day)
