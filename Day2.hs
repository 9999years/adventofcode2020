{-# LANGUAGE NoImplicitPrelude #-}

import Aoc (aoc, parseInt)
import qualified Data.Bifunctor
import qualified Data.Text as T
import Relude
import Text.Parsec as P
import Text.Parsec.Text

main :: IO ()
main = aoc parsePolicies part1 part2

part1 :: [(Policy, Text)] -> Int
part1 passwords =
  let isValid (Policy range char, password) =
        range `contains` (char `occursIn` password)
   in filter isValid passwords & length

part2 :: [(Policy, Text)] -> Int
part2 passwords =
  let isValid (Policy (i1, i2) char, password) =
        let oneIndex i = password `T.index` (i - 1)
         in (oneIndex i1 == char) `boolXor` (oneIndex i2 == char)
   in filter isValid passwords & length

contains :: (Int, Int) -> Int -> Bool
(lo, hi) `contains` val = val >= lo && val <= hi

occursIn :: Char -> Text -> Int
occursIn char = T.foldl (\n c -> if c == char then 1 + n else n) 0

boolXor :: Bool -> Bool -> Bool
a `boolXor` b = (a && not b) || (not a && b)

data Policy = Policy (Int, Int) Char deriving (Show)

parsePolicy :: Parser (Policy, Text)
parsePolicy = do
  lo <- parseInt
  char '-'
  hi <- parseInt
  char ' '
  policyChar <- lower
  string ": "
  password <- many1 lower
  return (Policy (lo, hi) policyChar, toText password)

parsePolicies :: Parser [(Policy, Text)]
parsePolicies =
  parsePolicy `endBy1` newline
