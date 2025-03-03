module Lib
    ( someFunc,
      minInt,
      example2
    ) where

import Data.List
import System.IO

minInt :: Int
minInt = minBound

someFunc :: IO ()
someFunc = putStrLn "my name"

times4 :: Int -> Int
times4 x = x * 4

double :: (a->a) -> a -> a
double f x = f (f x)


-- data MyPair a b = Pair a b  -- Defines a new type with a constructor
-- example :: MyPair Int String
-- example = Pair 42 "hello"

data MyPair = Mypair {
    fst :: Int,
    snd :: String
} deriving (Show)
example2 :: MyPair
example2 = Mypair 42 "hello"