{-# LANGUAGE FlexibleContexts #-}
module Common where

import Data.Map (Map)
import Text.Parsec
import Data.Functor.Identity

type Solution = String -> IO ()

-- | Other functions to offer on the cli
type Extra = String -> IO ()
type Extras = Map String (String -> IO ())

at :: [a] -> Int -> Maybe a
at [] _ = Nothing
at (x:xs) 0 = Just x
at (x:xs) n = at xs (n-1)
