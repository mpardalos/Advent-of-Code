{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Day4 where

import Common

import Text.Parsec
import Text.Parsec.Char as CharP
import qualified Data.Map as Map
import Data.Map (Map)
import Text.Read (readMaybe)
import Data.Maybe
import Data.Functor ((<&>), void)
import Data.Char

type Document = Map String String

valueP :: Parsec String () String
valueP = many1 (alphaNum <|> CharP.oneOf "#")
  <?> "value"

fieldP :: Parsec String () (String, String)
fieldP = do
  { fieldName <- many1 alphaNum <?> "field name"
  ; char ':'
  ; value <- valueP
  ; return (fieldName, value)
  } <?> "field"

documentP :: Parsec String () Document
documentP = Map.fromList <$> (fieldP `sepEndBy1` (CharP.space <|> CharP.endOfLine))
  <?> "document"

batchFileP :: Parsec String () [Document]
batchFileP = documentP `sepBy1` (void endOfLine <|> eof)
  <?> "batchFile"

isValid :: Document -> Bool
isValid doc = all (maybe False id) $
  [ f "byr" <&> (((==4) . length) &&& intBetween 1920 2002)
  , f "iyr" <&> (((==4) . length) &&& intBetween 2010 2020)
  , f "eyr" <&> (((==4) . length) &&& intBetween 2020 2030)
  , f "hgt" <&> validHeight

  , f "hcl" <&> \case
      ('#':hex) -> isJust (readMaybe @Int ("0x" ++ hex))
      _ -> False

  , f "ecl" <&> (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])

  , f "pid" <&> (((==9) . length) &&& (isJust . readMaybe @Int))
  ]
  where
    f n = Map.lookup n doc

    (&&&) f1 f2 a = f1 a && f2 a

    intBetween l h s = case readMaybe @Int s of
      Just n -> l <= n && n <= h
      Nothing -> False

    validHeight :: String -> Bool
    validHeight str = case span isDigit str of
      (numStr, "cm") -> intBetween 150 193 numStr
      (numStr, "in") -> intBetween 59 76 numStr
      _ -> False

solve :: Solution
solve input = do
  docs <- case parse batchFileP "input" input of
    Left err -> error (show err)
    Right docs -> pure docs
  putStr "Result: "; print (length (filter isValid docs))
