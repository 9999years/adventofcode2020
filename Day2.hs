{-# LANGUAGE NoImplicitPrelude #-}
import qualified Data.Text       as T
import           Relude
import           Relude.Function ((&))
import           Text.Read       (read)

import           Aoc             (aoc)

main :: IO ()
main = aoc parse part1 part2

part1 passwords =
  let isValid ((Policy range char), password) =
        range `contains` (char `occursIn` password)
   in length $ filter isValid passwords

part2 passwords =
  let isValid ((Policy (Range i1 i2) char), password) =
        let oneIndex i = password `T.index` (i - 1)
         in (oneIndex i1 == char) `boolXor` (oneIndex i2 == char)
   in length $ filter isValid passwords

data Range = Range Int Int deriving Show

contains :: Range -> Int -> Bool
(Range lo hi) `contains` val = val >= lo && val <= hi

data Policy = Policy Range Char deriving Show

parsePolicy :: Text -> (Policy, Text)
parsePolicy input =
  let (lo, input1) = T.breakOn (T.singleton '-') input
      (hi, input2) = T.breakOn (T.singleton ' ') (T.drop 1 input1)
      (char, input3) = T.breakOn (T.singleton ':') (T.drop 1 input2)
  in (Policy (Range ((read . toString) lo) ((read . toString) hi)) (T.head char), T.drop 2 input3)

parse :: Text -> [(Policy, Text)]
parse input =
  (\(policy, password) -> (policy, T.stripStart password)) <$> (parsePolicy <$> T.lines input)

occursIn :: Char -> Text -> Int
occursIn char = T.foldl (\n -> \c -> if c == char then 1 + n else n) 0

boolXor :: Bool -> Bool -> Bool
a `boolXor` b = (a && not b) || (not a && b)
