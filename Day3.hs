{-# LANGUAGE NoImplicitPrelude #-}

import Aoc (aoc, parseInt)
import Control.Category ((>>>))
import Control.Monad.Zip (mzip)
import qualified Data.Bifunctor
import qualified Data.Text as T
import Relude hiding (many)
import Text.Parsec as P
import Text.Parsec.Text (Parser)

main :: IO ()
main = aoc parsePuzzle part1 (const "unimplemented")

part1 :: NonEmpty (NonEmpty Tree) -> Int
part1 trees =
  let gridWidth = trees & head & length
   in trees `mzip` (0 :| [3, 6 ..] <&> (`mod` gridWidth))
        <&> ( uncurry (toList >>> (!!?))
                >>> maybe False isTree
                >>> fromEnum
            )
          & sum

--  <!!> trees)

parsePuzzle :: Parser (NonEmpty (NonEmpty Tree))
parsePuzzle =
  let parseTree :: Parser Tree
      parseTree = Tree <$> choice [char '#' >> return True, char '.' >> return False]
      parseLine :: Parser (NonEmpty Tree)
      parseLine = parseTree <:|> many parseTree
      (<:|>) = liftA2 (:|)
   in (parseLine <* newline) <:|> (parseLine `endBy` newline)

newtype Tree = Tree {isTree :: Bool} deriving (Show)
