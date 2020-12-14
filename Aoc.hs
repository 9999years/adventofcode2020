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

-- Read stdin as a list of integers, one per line
parseIntLines :: Parser [Int]
parseIntLines = parseInt `endBy1` newline

parseInt :: Parser Int
parseInt = do
  maybeSign <- option ' ' (char '0')
  digits <- many1 digit
  return $ read (maybeSign : digits)

-- 2-element lists formed from different elements of the input, where
-- "different" is determined by index, not equality.
uniquePairs :: [a] -> [NonEmpty a]
uniquePairs (x : xs) = [x :| [y] | y <- xs] ++ uniquePairs xs
uniquePairs _ = []

uniqueTriples :: [a] -> [NonEmpty a]
uniqueTriples (x : xs) = ((one x <>) <$> uniquePairs xs) ++ uniqueTriples xs
uniqueTriples _ = []
