{-# LANGUAGE NoImplicitPrelude #-}

import Aoc (aoc)
import qualified Data.Bifunctor
import qualified Data.Text as T
import Relude
import Text.Read (read)

main :: IO ()
main = aoc parse part1 part2

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

data Policy = Policy (Int, Int) Char deriving (Show)

parsePolicy :: Text -> (Policy, Text)
parsePolicy input =
  let (lo, input1) = T.breakOn (one '-') input
      (hi, input2) = T.breakOn (one ' ') (T.drop 1 input1)
      (char, input3) = T.breakOn (one ':') (T.drop 1 input2)
   in (Policy ((read . toString) lo, (read . toString) hi) (T.head char), T.drop 2 input3)

parse :: Text -> [(Policy, Text)]
parse input =
  Data.Bifunctor.second T.stripStart <$> (parsePolicy <$> lines input)

occursIn :: Char -> Text -> Int
occursIn char = T.foldl (\n c -> if c == char then 1 + n else n) 0

boolXor :: Bool -> Bool -> Bool
a `boolXor` b = (a && not b) || (not a && b)
