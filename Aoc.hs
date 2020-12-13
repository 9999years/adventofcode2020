{-# LANGUAGE NoImplicitPrelude #-}

module Aoc where

import           Data.Text.IO        (getContents)
import           Relude
import           Relude.Functor.Fmap ((<<$>>))
import           Text.Read           (read)

-- Run an Advent of Code puzzle, showing the part 1 and part 2 solutions.
aoc :: (Show b, Show c) => (Text -> a) -> (a -> b) -> (a -> c) -> IO ()
aoc parse part1 part2 =
  do input <- parse <$> getContents
     putStr "*   "
     print (part1 input)
     putStr "**  "
     print (part2 input)

-- Read stdin as a list of integers, one per line
intLines :: Text -> [Int]
intLines = (read . toString) <<$>> lines

-- 2-element lists formed from different elements of the input, where
-- "different" is determined by index, not equality.
uniquePairs :: [a] -> [NonEmpty a]
uniquePairs (x:xs) = [x :| [y] | y <- xs] ++ uniquePairs xs
uniquePairs _      = []

uniqueTriples :: [a] -> [NonEmpty a]
uniqueTriples (x:xs) = ((one x <>) <$> uniquePairs xs) ++ uniqueTriples xs
uniqueTriples _      = []
