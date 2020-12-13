{-# LANGUAGE NoImplicitPrelude #-}
import           Relude
import           Relude.Extra.Foldable1 (foldl1')
import           Relude.Function        ((&))
import           Relude.Unsafe          as Unsafe

import           Aoc                    (aoc, intLines, uniquePairs,
                                         uniqueTriples)

main :: IO ()
main = aoc intLines (sumTo2020Mult . uniquePairs) (sumTo2020Mult . uniqueTriples)

sumTo2020Mult entries = entries & filter ((== 2020) . sum) & Unsafe.head & product
