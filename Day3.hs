{-# LANGUAGE NoImplicitPrelude #-}

import Aoc (aoc)
import Control.Monad.Zip (mzip)
import Relude hiding (many)
import Text.Parsec as P
import Text.Parsec.Text (Parser)

newtype Tree = Tree {isTree :: Bool} deriving (Show)

main :: IO ()
main = aoc parsePuzzle part1 part2

part1 :: NonEmpty (NonEmpty Tree) -> Int
part1 = treesEncountered 3 1

part2 :: NonEmpty (NonEmpty Tree) -> Int
part2 trees =
  let treesEncountered' (right, down) = treesEncountered right down trees
   in [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)] <&> treesEncountered' & product

treesEncountered :: Int -> Int -> NonEmpty (NonEmpty Tree) -> Int
treesEncountered right down trees =
  -- How wide the map is; needed for horizontal-wrapping.
  let gridWidth = trees & head & length
      -- A list of x-indexes, one per element in `trees`, starting at 0 and
      -- increasing by `right` each step
      xIndexes = 0 :| [right, right * 2 ..] <&> (`mod` gridWidth)
   in -- Zip trees together with x-indexes
      (trees & keepEvery down) `mzip` xIndexes
        <&> ( uncurry (toList >>> (!!?))
                >>> maybe False isTree
                >>> fromEnum
            )
          & sum

-- Keep every `n`th element of a list; `keepEvery 1 = id`, `keepEvery 2` keeps
-- the elements at indexes 0,2,4...
keepEvery :: Int -> NonEmpty a -> NonEmpty a
keepEvery n (x :| xs) =
  let n' = n - 1
      rest = drop n' xs
   in case nonEmpty rest of
        Nothing -> one x
        Just rest' -> x :| toList (keepEvery n' rest')

parsePuzzle :: Parser (NonEmpty (NonEmpty Tree))
parsePuzzle =
  let parseTree = Tree <$> choice [char '#' >> return True, char '.' >> return False]
      parseLine = parseTree <:|> many parseTree
      (<:|>) = liftA2 (:|)
   in (parseLine <* newline) <:|> (parseLine `endBy` newline)
