{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Aoc (aoc)
import Relude hiding ((<|>))
import Text.Parsec as P
import Text.Parsec.Text (Parser)

main :: IO ()
main = aoc parsePuzzle part1 (const "unimplemented")

part1 :: [PartialPassport] -> Int
part1 passports = passports <&> fromPartialPassport & catMaybes & length

data PassportField
  = BirthYear
  | IssueYear
  | ExpirationYear
  | Height
  | HairColor
  | EyeColor
  | PassportId
  | CountryId

data Passport = Passport
  { birthYear :: Text,
    issueYear :: Text,
    expirationYear :: Text,
    height :: Text,
    hairColor :: Text,
    eyeColor :: Text,
    passportId :: Text,
    countryId :: Maybe Text
  }
  deriving (Show)

fromPartialPassport :: PartialPassport -> Maybe Passport
fromPartialPassport
  PartialPassport
    { birthYear,
      issueYear,
      expirationYear,
      height,
      hairColor,
      eyeColor,
      passportId,
      countryId
    } = do
    birthYear' <- birthYear
    issueYear' <- issueYear
    expirationYear' <- expirationYear
    height' <- height
    hairColor' <- hairColor
    eyeColor' <- eyeColor
    passportId' <- passportId
    return
      Passport
        { birthYear = birthYear',
          issueYear = issueYear',
          expirationYear = expirationYear',
          height = height',
          hairColor = hairColor',
          eyeColor = eyeColor',
          passportId = passportId',
          countryId = countryId
        }

data PartialPassport = PartialPassport
  { birthYear :: Maybe Text,
    issueYear :: Maybe Text,
    expirationYear :: Maybe Text,
    height :: Maybe Text,
    hairColor :: Maybe Text,
    eyeColor :: Maybe Text,
    passportId :: Maybe Text,
    countryId :: Maybe Text
  }
  deriving (Show)

emptyPartialPassport :: PartialPassport
emptyPartialPassport =
  PartialPassport
    { birthYear = Nothing,
      issueYear = Nothing,
      expirationYear = Nothing,
      height = Nothing,
      hairColor = Nothing,
      eyeColor = Nothing,
      passportId = Nothing,
      countryId = Nothing
    }

setField :: PartialPassport -> (PassportField, Text) -> PartialPassport
setField passport (field, value) =
  let value' = Just value
   in case field of
        BirthYear -> passport {birthYear = value'}
        IssueYear -> passport {issueYear = value'}
        ExpirationYear -> passport {expirationYear = value'}
        Height -> passport {height = value'}
        HairColor -> passport {hairColor = value'}
        EyeColor -> passport {eyeColor = value'}
        PassportId -> passport {passportId = value'}
        CountryId -> passport {countryId = value'}

parsePuzzle :: Parser [PartialPassport]
parsePuzzle = parsePartialPassport `sepBy1` many1 (char '\n')

parsePartialPassport :: Parser PartialPassport
parsePartialPassport = do
  fields <- parseFieldPair `sepEndBy1` oneOf " \n"
  fields
    & foldl' setField emptyPartialPassport
    & return

parseFieldPair :: Parser (PassportField, Text)
parseFieldPair = do
  field <- parseField
  char ':'
  value <- many1 $ noneOf " \n"
  return (field, toText value)

parseField :: Parser PassportField
parseField =
  try (string "byr" >> return BirthYear)
    <|> try (string "iyr" >> return IssueYear)
    <|> try (string "eyr" >> return ExpirationYear)
    <|> try (string "hgt" >> return Height)
    <|> try (string "hcl" >> return HairColor)
    <|> try (string "ecl" >> return EyeColor)
    <|> try (string "pid" >> return PassportId)
    <|> try (string "cid" >> return CountryId)
