{-# LANGUAGE NoImplicitPrelude #-}

import Aoc (aoc, parseInt)
import Relude
import Relude.Unsafe as Unsafe (head)
import Text.Parsec
import Text.Parsec.Text (Parser)

main :: IO ()
main = aoc parseIntLines (sumTo2020Mult . uniquePairs) (sumTo2020Mult . uniqueTriples)

sumTo2020Mult entries =
  entries
    & filter ((== 2020) . sum)
    & Unsafe.head
    & product

parseIntLines :: Parser [Int]
parseIntLines = parseInt `endBy1` newline

-- 2-element lists formed from different elements of the input, where
-- "different" is determined by index, not equality.
uniquePairs :: [a] -> [NonEmpty a]
uniquePairs (x : xs) = [x :| [y] | y <- xs] ++ uniquePairs xs
uniquePairs _ = []

uniqueTriples :: [a] -> [NonEmpty a]
uniqueTriples (x : xs) = ((one x <>) <$> uniquePairs xs) ++ uniqueTriples xs
uniqueTriples _ = []
