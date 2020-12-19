{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Aoc (aoc, contains, parseInt)
import Numeric (readHex)
import Relude hiding ((<|>))
import Text.Parsec as P
import Text.Parsec.Text (Parser)

main :: IO ()
main = aoc parsePuzzle part1 part2

part1 :: [PartialPassport1] -> Int
part1 passports = passports <&> fromPartialPassport1 & catMaybes & length

part2 :: [PartialPassport1] -> Int
part2 passports =
  passports <&> fromPartialPassport1 & catMaybes
    <&> fromPartialPassport2 & rights & length

data PassportField
  = BirthYear
  | IssueYear
  | ExpirationYear
  | Height
  | HairColor
  | EyeColor
  | PassportId
  | CountryId

data Height = Centimeter Int | Inch Int
  deriving (Show)

parseHeight :: Parser Height
parseHeight =
  let unitConstraint :: String -> (Int, Int) -> (Int -> Height) -> Int -> Parser Height
      unitConstraint tok range constructor value =
        string tok
          >> if range `contains` value
            then return $ constructor value
            else
              fail $
                "Heights in " ++ tok ++ " must be in the range "
                  ++ show range
                  ++ ", inclusive"
   in do
        value <- parseInt
        unitConstraint "cm" (150, 193) Centimeter value
          <|> unitConstraint "in" (59, 76) Inch value

data Rgb8 = Rgb8
  { red :: Word8,
    green :: Word8,
    blue :: Word8
  }
  deriving (Show)

parseRgb8 :: Parser Rgb8
parseRgb8 =
  let parseHex :: String -> Parser Int
      parseHex s =
        case readHex s & filter (snd >>> length >>> (== 0)) & map fst of
          [] -> fail $ "Invalid hex number " ++ s
          (num : _) -> return num
      hexByte :: Parser Word8
      hexByte = count 2 hexDigit >>= parseHex <&> toEnum
   in do
        char '#' <?> "hex color"
        red <- hexByte
        green <- hexByte
        blue <- hexByte
        return
          Rgb8
            { red = red,
              green = green,
              blue = blue
            }

data EyeColor = Amber | Blue | Brown | Gray | Green | Hazel | OtherEyeColor
  deriving (Show)

parseEyeColor :: Parser EyeColor
parseEyeColor =
  tryString "amb" Amber
    <|> tryString "blu" Blue
    <|> tryString "brn" Brown
    <|> tryString "gry" Gray
    <|> tryString "grn" Green
    <|> tryString "hzl" Hazel
    <|> tryString "oth" OtherEyeColor

data PartialPassport3 = PartialPassport3
  { birthYear :: Int,
    issueYear :: Int,
    expirationYear :: Int,
    height :: Height,
    hairColor :: Rgb8,
    eyeColor :: EyeColor,
    passportId :: Text,
    countryId :: Maybe Text
  }
  deriving (Show)

fromPartialPassport2 :: PartialPassport2 -> Either Text PartialPassport3
fromPartialPassport2
  PartialPassport2
    { birthYear,
      issueYear,
      expirationYear,
      height,
      hairColor,
      eyeColor,
      passportId,
      countryId
    } =
    let adaptParser :: Parser a -> Text -> Either Text a
        adaptParser parser input = parse (parser <* eof) "" input & mapLeft show
     in do
          birthYear' <- birthYear & toString & readBoundedInt (1920, 2002)
          issueYear' <- issueYear & toString & readBoundedInt (2010, 2020)
          expirationYear' <- expirationYear & toString & readBoundedInt (2020, 2030)
          height' <- height & adaptParser (parseHeight <?> "height")
          hairColor' <- hairColor & adaptParser parseRgb8
          eyeColor' <- eyeColor & adaptParser (parseEyeColor <?> "eye color")
          passportId' <- passportId & adaptParser (count 9 digit <?> "9-digit number") <&> toText
          return
            PartialPassport3
              { birthYear = birthYear',
                issueYear = issueYear',
                expirationYear = expirationYear',
                height = height',
                hairColor = hairColor',
                eyeColor = eyeColor',
                passportId = passportId',
                countryId = countryId
              }

readBoundedInt :: (Int, Int) -> String -> Either Text Int
readBoundedInt (lo, hi) str = do
  int <- readEither str
  if lo <= int && hi >= int
    then return int
    else Left $ toText $ "value must be between " ++ show lo ++ " and " ++ show hi

data PartialPassport2 = PartialPassport2
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

fromPartialPassport1 :: PartialPassport1 -> Maybe PartialPassport2
fromPartialPassport1
  PartialPassport1
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
      PartialPassport2
        { birthYear = birthYear',
          issueYear = issueYear',
          expirationYear = expirationYear',
          height = height',
          hairColor = hairColor',
          eyeColor = eyeColor',
          passportId = passportId',
          countryId = countryId
        }

data PartialPassport1 = PartialPassport1
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

emptyPartialPassport :: PartialPassport1
emptyPartialPassport =
  PartialPassport1
    { birthYear = Nothing,
      issueYear = Nothing,
      expirationYear = Nothing,
      height = Nothing,
      hairColor = Nothing,
      eyeColor = Nothing,
      passportId = Nothing,
      countryId = Nothing
    }

setField :: PartialPassport1 -> (PassportField, Text) -> PartialPassport1
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

parsePuzzle :: Parser [PartialPassport1]
parsePuzzle = parsePartialPassport `sepBy1` many1 (char '\n')

parsePartialPassport :: Parser PartialPassport1
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
  tryString "byr" BirthYear
    <|> tryString "iyr" IssueYear
    <|> tryString "eyr" ExpirationYear
    <|> tryString "hgt" Height
    <|> tryString "hcl" HairColor
    <|> tryString "ecl" EyeColor
    <|> tryString "pid" PassportId
    <|> tryString "cid" CountryId

tryString :: String -> a -> Parser a
tryString tok ret = try (string tok >> return ret)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft _ (Right x) = Right x
mapLeft f (Left x) = Left (f x)
