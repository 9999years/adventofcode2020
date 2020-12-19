{-# LANGUAGE NoImplicitPrelude #-}

module Aoc where

import Data.Text.IO (getContents)
import Relude
import Text.Parsec as P
import Text.Parsec.Text
import Text.Read (read)

-- Run an Advent of Code puzzle, showing the part 1 and part 2 solutions.
aoc :: (Show b, Show c) => Parser a -> (a -> b) -> (a -> c) -> IO ()
aoc parser part1 part2 =
  do
    rawInput <- getContents
    case parse parser "" rawInput of
      Left err -> do
        putStr "Error parsing puzzle input: "
        print err
      Right input -> do
        putStr "*   "
        print (part1 input)
        putStr "**  "
        print (part2 input)

parseInt :: Parser Int
parseInt = do
  maybeSign <- option ' ' (char '0')
  digits <- many1 digit
  return $ read (maybeSign : digits)

contains :: (Int, Int) -> Int -> Bool
(lo, hi) `contains` val = val >= lo && val <= hi
