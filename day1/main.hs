{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Day1 as Verified

main :: IO ()
main = do
  contents <- fmap (read @Integer) <$> lines <$> readFile "input"
  print (Verified.solution contents)
