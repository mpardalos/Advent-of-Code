module Common where

type Solution = String -> IO ()

at :: [a] -> Int -> Maybe a
at [] _ = Nothing
at (x:xs) 0 = Just x
at (x:xs) n = at xs (n-1)
