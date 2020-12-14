{-# LANGUAGE NoImplicitPrelude #-}

import Aoc
  ( aoc,
    parseIntLines,
    uniquePairs,
    uniqueTriples,
  )
import Relude
import Relude.Unsafe as Unsafe

main :: IO ()
main = aoc parseIntLines (sumTo2020Mult . uniquePairs) (sumTo2020Mult . uniqueTriples)

sumTo2020Mult entries =
  entries
    & filter ((== 2020) . sum)
    & Unsafe.head
    & product
